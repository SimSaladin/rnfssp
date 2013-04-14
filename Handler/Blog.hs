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
import Data.Time (getCurrentTime)
import qualified Data.Conduit         as C
import qualified Data.Conduit.List    as CL
import qualified Data.Text as T
import           Data.Time (formatTime)
import           System.Locale (defaultTimeLocale)
import           Yesod.Markdown (Markdown(..), markdownField, markdownToHtml)
import           Database.Persist.GenericSql (rawSql)
import Chat


-- * Viewing

renderView :: Blogpost -> Widget -> Handler RepHtml
renderView post x = defaultLayout $ do
    setTitle $ toHtml (blogpostTitle post) `mappend` " - SS Blog"
    renderBlog (blogpostTitle post) x

-- | 
getWidgets :: Entity Blogpost
           -> Handler (Widget, Widget) -- ^ (Blogpost Widget, comments Widget)
getWidgets (Entity key val) = do
    comments <- runDB $ selectList [BlogcommentPost ==. key] [Desc BlogcommentTime]
    post     <- liftM (blogpostWidget val (length comments) False) isAdmin'
    return (post, blogcommentsWidget comments)

getPostPreview :: Entity Blogpost -> Widget
getPostPreview (Entity key val) = do
    comments <- lift $ runDB $ count [BlogcommentPost ==. key]
    blogpostWidget val comments True =<< lift isAdmin'

-- | Home redirects to the newest blog post.
getBlogHomeR :: Handler RepHtml
getBlogHomeR = do
    mpost <- runDB $ selectFirst [] [Desc BlogpostTime]
    maybe noPosts (redirect . BlogViewR . blogpostUrlpath . entityVal) mpost
  where
    noPosts = defaultLayout $ do
        setTitle "Blog: No posts."
        navigation "Blog"
        renderBlog "" $ [whamlet|<i>No posts!|]

-- | View a post specified in the url.
getBlogViewR :: Text -> Handler RepHtml
getBlogViewR path = do
    e                 <- runDB . getBy404 $ UniqueBlogpost path
    (widget, encType) <- generateFormPost $ commentForm (entityKey e) Nothing
    (post, comments)  <- getWidgets e
    renderView (entityVal e) $(widgetFile "blog-view")
  where
    result = FormMissing

-- | Post action in a view is for posting comments.
postBlogViewR :: Text -> Handler RepHtml
postBlogViewR path = do
    e <- runDB $ getBy404 $ UniqueBlogpost path
    ((result, widget), encType) <- runFormPost (commentForm (entityKey e) Nothing)
    case result of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            setMessage "Comment sent."
            redirect $ BlogViewR path
        _  -> do
            (post, comments) <- getWidgets e
            renderView (entityVal e) $(widgetFile "blog-view")

-- | 
getBlogTagR :: Text -> Handler RepHtml
getBlogTagR tag = do
    posts <- runDB $ rawSql query [toPersistValue $ "%\"s" `mappend` tag `mappend` "\"%"]
    defaultLayout $ do
        setTitle $ toHtml $ "Posts under " `mappend` tag
        renderBlog "" $ [whamlet|
\<i>Found #{length posts} posts under</i> #{tag}.
|] >> mapM_ getPostPreview posts
  where
    query = "SELECT ?? FROM \"blogpost\" WHERE \"tags\" SIMILAR TO ? ORDER BY \"time\""

-- * Publishing

renderPublish :: Widget -> Handler RepHtml
renderPublish x = defaultLayout $ do
    setTitle "Blog - Publishing"
    renderBlog "" x

getBlogAddR :: Handler RepHtml
getBlogAddR = do
    Entity _ user <- requireAuth
    (widget, encType) <- generateFormPost (blogpostForm user Nothing) -- new post form
    renderPublish $ renderFormH (submitButton "Publish") MsgBlogPublish BlogAddR FormMissing widget encType

postBlogAddR :: Handler RepHtml
postBlogAddR = do
    Entity _ user <- requireAuth
    ((result, widget), encType) <- runFormPost (blogpostForm user Nothing) -- new post form
    case result of
        FormSuccess post -> do
            _ <- runDB $ insert post
            setMessage $ toHtml $ "Post " `mappend` blogpostTitle post `mappend` " published."
            redirect $ BlogViewR $ blogpostUrlpath post
        _ -> renderPublish $
            renderFormH (submitButton "Publish") MsgBlogPublish BlogAddR result widget encType


-- * Edit

renderEdit :: Blogpost -> Widget -> Handler RepHtml
renderEdit post x = defaultLayout $ do
    setTitle $ "[Edit] " `mappend` toHtml (blogpostTitle post) `mappend` " - SS Blog"
    renderBlog (blogpostTitle post) x

getBlogEditR :: Text -> Handler RepHtml
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

postBlogEditR :: Text -> Handler RepHtml
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
        FormFailure _ -> do
            Entity _ val <- runDB $ getBy404 $ UniqueBlogpost path
            renderEdit val $ renderFormH
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


-- * Helpers

getParam :: Text -> Text -> Handler Text
getParam which fallback = fromGet
  where fromGet = lookupGetParam which >>= maybe fromCookie return
        fromCookie = liftM (fromMaybe fallback) $ lookupCookie which

-- | Render a preview of the most recent post.
newestPost :: Widget
newestPost = do
    posts <- lift $ runDB $ selectList [] [Desc BlogpostTime, LimitTo 3]
    case posts of
        []                    -> mempty
        (Entity key val : xs) -> do
            editing       <- lift isAdmin'
            commentCount  <- lift $ runDB $ count [BlogcommentPost ==. key]
            blogpostWidget val commentCount True editing
            [whamlet|
<h6>Previous posts:
<ul .list-clean>
$forall Entity _ post <- xs
  <li>
    <a href=@{BlogViewR $ blogpostUrlpath post}>#{blogpostTitle post}
|]

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


-- * Widgets

-- | Wrap blog content with chat and tags.
renderBlog :: Text -> Widget -> Widget
renderBlog current content = do
    navigation "Blog"
    $(widgetFile "blog-wrapper")

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

renderTagcloud :: Text -> Widget
renderTagcloud current = do
    (posts, tagcloud) <- lift $ runDB $ C.runResourceT $ selectSource [] [Asc BlogpostTime]
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


-- * Forms
blogpostForm :: User -> Maybe Blogpost -> Form Blogpost
blogpostForm poster mp = renderBootstrap $ mkBlogpost
    <$> aformM (liftIO getCurrentTime)
    <*> areq textField     "Title"              (blogpostTitle    <$> mp)
    <*> areq urlpathField  "Unique URL part"    (blogpostUrlpath  <$> mp)
    <*> areq tagField      "Tags"               (blogpostTags     <$> mp)
    <*> areq markdownField "Content (Markdown)" (blogpostMarkdown <$> mp)
  where
    mkBlogpost time title url tags md = Blogpost
        (maybe                  time blogpostTime   mp)
        (maybe (userUsername poster) blogpostPoster mp)
        title url md tags
        (markdownToHtml md)
        (markdownToHtml $ getPreview md)
        (md == getPreview md)

    urlpathField   = checkM validUrlpart textField
    validUrlpart u = let
        toCheck = T.toLower u
        isLegal = isNothing $ T.find (\x -> not ( C.isAsciiLower x || C.isDigit x || x == '-' || x == '_')) toCheck
        in do
            dbentry <- runDB $ selectFirst [BlogpostPoster ==. toCheck] []
            let ret | isJust dbentry = Left ("Post with url already exists"::Text)
                    | not isLegal    = Left "URL part contains illegal characters"
                    | otherwise      = Right toCheck
                in return ret


tagField :: Field App App [Text]
tagField = checkMMap f T.unwords textField
  where f :: Text -> GHandler App App (Either Text [Text])
        f = return . Right . T.words

commentForm :: BlogpostId          -- ^ main post
            -> Maybe BlogcommentId -- ^ Maybe parent comment
            -> Form  Blogcomment
commentForm pid mcid = renderBootstrap $ Blogcomment
    <$> pure pid
    <*> pure mcid
    <*> aformM timeNow
    <*> pure Nothing --TODO user creds checking
    <*> areq textField "Name" Nothing
    <*> aopt urlField "Webpage" Nothing
    <*> areq textareaField "Message" Nothing
