module Handler.Blog
    ( getBlogHomeR
    , getBlogViewR, postBlogViewR
    , getBlogEditR, postBlogEditR
    , getBlogAddR,  postBlogAddR
    , getBlogTagR
    , newestPost
    ) where

import           Utils
import           Import
import qualified Data.List as L
import qualified Data.Char as C
import           Data.Maybe
import qualified Data.Conduit         as C
import qualified Data.Conduit.List    as CL
import qualified Data.Text as T
import           Data.Time (formatTime, getCurrentTime)
import           System.Locale (defaultTimeLocale)
import           Yesod.Markdown (Markdown(..), markdownField, markdownToHtml)
import           Database.Persist.Sql (rawSql)
import Chat


-- * Render combinators

-- | Adds chat and tags navigation.
renderBlog :: Text -> Widget -> Widget
renderBlog current content = do
    navigation "Blog"
    $(widgetFile "blog-wrapper")

-- | Viewing single post.
renderView :: Blogpost -> Widget -> Handler Html
renderView post x = defaultLayout $ do
    setTitle $ toHtml (blogpostTitle post) <> " at animu"
    renderBlog (blogpostTitle post) x

-- | Editing.
renderPublish :: Widget -> Handler Html
renderPublish x = defaultLayout $ do
    setTitle "Blog - Publishing"
    renderBlog "" x

renderEdit :: Blogpost -> Widget -> Handler Html
renderEdit post x = defaultLayout $ do
    setTitle $ "[Edit] " <> toHtml (blogpostTitle post) <> " - SS Blog"
    renderBlog (blogpostTitle post) x


-- * Viewing, listing and commenting posts

-- | Home redirects to the newest blog post.
getBlogHomeR :: Handler Html
getBlogHomeR = do
    mpost <- runDB $ selectFirst [BlogpostPublic ==. True] [Desc BlogpostTime]
    maybe noPosts (redirect . BlogViewR . blogpostUrlpath . entityVal) mpost
  where
    noPosts = defaultLayout $ do
        setTitle "Blog: No posts."
        renderBlog "Blog" emptyBlogWidget

-- | View a post specified in the url.
getBlogViewR :: Text -> Handler Html
getBlogViewR path = do
    showHidden <- isAdmin'
    e                 <- runDB . getBy404 $ UniqueBlogpost path
    if (blogpostPublic $ entityVal e) || showHidden 
      then do
          (widget, encType) <- generateFormPost $ commentForm (entityKey e) Nothing
          (post, comments)  <- generateView e
          renderView (entityVal e) $(widgetFile "blog-view")
      else permissionDenied "Need admin access to show hidden blog posts."
  where
    result = FormMissing

-- | Post action in a view is for posting comments.
postBlogViewR :: Text -> Handler Html
postBlogViewR path = do
    e <- runDB $ getBy404 $ UniqueBlogpost path
    ((result, widget), encType) <- runFormPost (commentForm (entityKey e) Nothing)
    case result of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            setMessage "Comment sent."
            redirect $ BlogViewR path
        _  -> do
            (post, comments) <- generateView e
            renderView (entityVal e) $(widgetFile "blog-view")

-- | Render a preview of the most recent post.
newestPost :: Widget
newestPost = do
    posts <- liftHandlerT $ runDB $ selectList [BlogpostPublic ==. True] [Desc BlogpostTime, LimitTo 3]
    case posts of
        []                    -> emptyBlogWidget
        (Entity key val : xs) -> do
            editing       <- liftHandlerT isAdmin'
            commentCount  <- liftHandlerT $ runDB $ count [BlogcommentPost ==. key]
            blogpostWidget val commentCount True editing
            [whamlet|
<h6>Previous posts:
<ul .list-clean>
$forall Entity _ post <- xs
  <li>
    <a href=@{BlogViewR $ blogpostUrlpath post}>#{blogpostTitle post}
|]

emptyBlogWidget :: Widget
emptyBlogWidget = [whamlet|
<h1>Title h1
<h2>Mui.
<h3>h3 here
<h4>and h4
<h5>...and h5
<h6>Newest post
<p>There could be a block post here. But since there are no posts, we only show #
   header h1-h6 test here. Click here to write one :)
    |]

generateView :: Entity Blogpost -> Handler (Widget, Widget) -- ^ (Blogpost Widget, comments Widget)
generateView (Entity key val) = do
    comments <- runDB $ selectList [BlogcommentPost ==. key] [Desc BlogcommentTime]
    post     <- liftM (blogpostWidget val (length comments) False) isAdmin'
    return (post, blogcommentsWidget comments)

generatePreview :: Entity Blogpost -> Widget
generatePreview (Entity key val) = do
    comments <- liftHandlerT $ runDB $ count [BlogcommentPost ==. key]
    blogpostWidget val comments True =<< liftHandlerT isAdmin'

-- | A single post.
-- TODO: 3 formatTime's really needed?
blogpostWidget :: Blogpost -- ^ Post to render.
               -> Int      -- ^ Number of comments.
               -> Bool     -- ^ Render as preview?
               -> Bool     -- ^ Include editing shortcuts?
               -> Widget
blogpostWidget post commentCount preview editing = $(widgetFile "blog-post")
      where title    = blogpostTitle post
            poster   = blogpostPoster post
            link     = BlogViewR $ blogpostUrlpath post
            month    = formatTime defaultTimeLocale "%b" (blogpostTime post)
            day      = formatTime defaultTimeLocale "%d" (blogpostTime post)
            year     = formatTime defaultTimeLocale "%Y" (blogpostTime post)

blogcommentsWidget :: [Entity Blogcomment] -> Widget
blogcommentsWidget comments =
    let (roots', children)  = part Nothing comments
    in case roots' of
        []      -> [whamlet|<i>No comments.|]
        parents -> $(widgetFile "blog-comments")
  where
    part                mpid           = L.partition ((==) mpid . blogcommentParent . entityVal)
    blogcommentsWidget' mpid children' =
      let (parents, children)          = part mpid children'
      in $(widgetFile "blog-comments")

commentForm :: BlogpostId          -- ^ main post
            -> Maybe BlogcommentId -- ^ Maybe parent comment
            -> Form  Blogcomment
commentForm pid mcid = renderBootstrap $ Blogcomment
    <$> pure pid
    <*> pure mcid
    <*> lift timeNow
    <*> pure Nothing --TODO user creds checking
    <*> areq textField "Name" Nothing
    <*> aopt urlField "Webpage" Nothing
    <*> areq textareaField "Message" Nothing

-- ** Tags

-- | Retrieve posts tagged under @tag@.
getBlogTagR :: Text -> Handler Html
getBlogTagR tag = do
    posts <- runDB $ rawSql query [toPersistValue $ "%\"s" <> tag <> "\"%"]
    defaultLayout $ do
        setTitle $ toHtml $ "Posts under " <> tag
        renderBlog "" $ [whamlet|
\<i>Found #{length posts} posts under</i> #{tag}.
|] >> mapM_ generatePreview posts
  where
    query = "SELECT ?? FROM \"blogpost\" WHERE \"public\" = true AND \"tags\" SIMILAR TO ? ORDER BY \"time\""

renderTagcloud :: Text -> Widget
renderTagcloud current = do
    (posts, tagcloud) <- liftHandlerT $ runDB $ C.runResourceT $ selectSource [BlogpostPublic ==. True] [Asc BlogpostTime]
        C.$$  CL.fold (\(x, x') p -> (sortMonth x p, addTags x' $ entityVal p)) ([], [])
    $(widgetFile "blog-navigation")
  where
    addTags [] = blogpostTags
    addTags xs = L.union xs . blogpostTags
    getMonth   = formatTime defaultTimeLocale "%B %Y" . blogpostTime . entityVal

    sortMonth ys@((month, xs) : ys') x = let
        month' = getMonth x
        in if month == month'
            then (month , x : xs) : ys'
            else (month',    [x]) : ys
    sortMonth                     [] x = [(getMonth x, [x])]

tagField :: Field (HandlerT App IO) [Text]
tagField = checkMMap f T.unwords textField
  where f :: Text -> Handler (Either Text [Text])
        f = return . Right . T.words

-- * Content management

getBlogAddR :: Handler Html
getBlogAddR = do
    Entity _ user <- requireAuth
    (widget, encType) <- generateFormPost (blogpostForm user Nothing) -- new post form
    renderPublish $ renderFormH (submitButton "Publish") MsgBlogPublish BlogAddR FormMissing widget encType

postBlogAddR :: Handler Html
postBlogAddR = do
    Entity _ user <- requireAuth
    ((result, widget), encType) <- runFormPost (blogpostForm user Nothing) -- new post form
    case result of
        FormSuccess post -> do
            _ <- runDB $ insert post
            setMessage $ toHtml $ "Post " <> blogpostTitle post <> " published."
            redirect $ BlogViewR $ blogpostUrlpath post
        _ -> renderPublish $
            renderFormH (submitButton "Publish") MsgBlogPublish BlogAddR result widget encType

getBlogEditR :: Text -> Handler Html
getBlogEditR path = do
    Entity _ user     <- requireAuth
    Entity _ val      <- runDB $ getBy404 $ UniqueBlogpost path
    (widget, encType) <- generateFormPost (blogpostForm user (Just val))
    renderEdit val $ do
        [whamlet|<h1>Editing #{path}|]
        renderFormH (submitButton "Publish")
                    MsgBlogPublish
                    (BlogEditR path)
                    result widget encType
    where result = FormMissing

postBlogEditR :: Text -> Handler Html
postBlogEditR path = do
    Entity _ user <- requireAuth
    Entity _ val  <- runDB $ getBy404 $ UniqueBlogpost path
    ((result, widget), encType) <- runFormPost $ blogpostForm user (Just val)
    case result of
        FormSuccess post -> if blogpostUrlpath post /= path
            then invalidUrl
            else do
                updatePost post
                setMessage "Succesfully edited."
                redirect $ BlogViewR path
        FormFailure _ -> renderEdit val $ renderFormH
                (submitButton "Publish")
                MsgBlogPublish
                (BlogEditR path)
                result widget encType
        FormMissing -> redirect $ BlogEditR path
  where
    invalidUrl = do
        setMessage "The url part should not be changed."
        redirect $ BlogEditR path
    updatePost post = runDB $ do
        Entity key _ <- getBy404 $ UniqueBlogpost path
        replace key post

-- * Forms
blogpostForm :: User -> Maybe Blogpost -> Form Blogpost
blogpostForm poster mp = renderBootstrap $ mkBlogpost
    <$> lift (liftIO getCurrentTime)
    <*> areq textField     "Title"              (blogpostTitle    <$> mp)
    <*> areq urlpathField  "Unique URL part"    (blogpostUrlpath  <$> mp)
    <*> areq tagField      "Tags"               (blogpostTags     <$> mp)
    <*> areq boolField     "Public"             (blogpostPublic   <$> mp)
    <*> areq markdownField "Content (Markdown)" (blogpostMarkdown <$> mp)
  where
    mkBlogpost time title url tags public md = Blogpost
        (maybe                  time blogpostTime   mp)
        (maybe (userUsername poster) blogpostPoster mp)
        title url tags public md
        (markdownToHtml md)
        (markdownToHtml $ getPreview md)
        (md == getPreview md)

    urlpathField   = checkM validUrlpart textField
    validUrlpart u = let
        toCheck = T.toLower u
        isLegal = isNothing $ T.find (\x -> not ( C.isAsciiLower x || C.isDigit x || x == '-' || x == '_')) toCheck
        in do
            dbentry <- runDB $ selectFirst [BlogpostUrlpath ==. toCheck] []
            let ret | isJust dbentry = Left ("Post with url already exists"::Text)
                    | not isLegal    = Left "URL part contains illegal characters"
                    | otherwise      = Right toCheck
                in return ret

-- * Helpers

-- | Accumulating param fetching:
--  * try GET param
--  * try cookie
--  * return fallback
getParam :: Text -> Text -> Handler Text
getParam which fallback = fromGet
  where fromGet = lookupGetParam which >>= maybe fromCookie return
        fromCookie = liftM (fromMaybe fallback) $ lookupCookie which

-- | Page based navigation in some content.
pageNav :: [Int]     -- ^ Available pages.
        -> Int       -- ^ Current page.
        -> Int       -- ^ Limit per page.
        -> Route App -- ^ Route for links.
        -> Widget
pageNav pages current limit route = [whamlet|
$if length pages > 1
    <div .nav-three>
        <span>
            $if current > 1
                <a href=@{route}?per_page=#{limit}&page=#{current - 1}>Newer
            $else
                &nbsp;
        <span>
            $if current < length pages
                <a href=@{route}?per_page=#{limit}&page=#{current + 1}>Older
            $else
                &nbsp;
        <span>
            $forall n <- pages
                $if n == current
                    <span>#{n}
                $else
                    <a href=@{route}?per_page=#{limit}&page=#{n}> #{n}
|]

getPreview :: Markdown -> Markdown
getPreview = Markdown . fst . T.breakOn "\n\n" . unMarkdown

