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
    setTitle . toHtml $ blogpostTitle post <> " at animu"
    renderBlog (blogpostTitle post) x

-- | Editing.
renderPublish :: Widget -> Handler Html
renderPublish x = defaultLayout $ do
    setTitle "Blog - Publishing"
    renderBlog "" x

renderEdit :: Blogpost -> Widget -> Handler Html
renderEdit post x = defaultLayout $ do
    setTitle . toHtml $ "[Edit] " <> blogpostTitle post <> " - SS Blog"
    renderBlog (blogpostTitle post) x

-- * Viewing, listing and commenting posts

-- | Home redirects to the newest blog post.
getBlogHomeR :: Handler Html
getBlogHomeR = runDB (selectFirst [BlogpostPublic ==. True] [Desc BlogpostTime])
    >>= maybe noPosts (redirect . BlogViewR . blogpostUrlpath . entityVal)
    where
        noPosts = defaultLayout $ do
            setTitle "Blog: No posts."
            renderBlog "Blog" emptyBlogWidget

-- | View a post specified in the url.
getBlogViewR :: Text -> Handler Html
getBlogViewR path = do
    showHidden <- isAdmin'
    Entity k v <- runDB . getBy404 $ UniqueBlogpost path
    if blogpostPublic v || showHidden 
      then do
          (widget, encType) <- generateFormPost $ commentForm k Nothing
          (post, comments)  <- generateView (Entity k v)
          let result = FormMissing
              in renderView v $(widgetFile "blog-view")
      else permissionDenied "Need admin access to show hidden blog posts."

-- | Post action in a view is for posting comments.
postBlogViewR :: Text -> Handler Html
postBlogViewR path = do
    e <- runDB $ getBy404 $ UniqueBlogpost path
    ((result, widget), encType) <- runFormPost (commentForm (entityKey e) Nothing)
    case result of
        FormSuccess comment -> do
            void . runDB $ insert comment
            setMessage "Comment sent."
            redirect $ BlogViewR path
        _  -> do
            (post, comments) <- generateView e
            renderView (entityVal e) $(widgetFile "blog-view")

-- | Render a preview of the most recent post.
newestPost :: Widget
newestPost = do
    posts <- liftHandlerT . runDB $ selectList [BlogpostPublic ==. True] [Desc BlogpostTime, LimitTo 4]
    if' (null posts) emptyBlogWidget $ let (Entity k v : xs) = posts in do
        (canEdit, comment_c) <- liftHandlerT $
            liftM2 (,) isAdmin' (runDB $ count [BlogcommentPost ==. k])
        blogpostWidget v comment_c True canEdit

        -- listing of previous posts, and their info.
        times <- liftIO $ mapM (formatTimeZoned "%d.%m.%y". blogpostTime . entityVal) xs
        [whamlet|
<h6>Previous posts:
<ul .list-clean>
$forall (Entity _ post, time) <- zip xs times
  <li>
    <a href=@{BlogViewR $ blogpostUrlpath post}>#{blogpostTitle post}
    <b>#{time}
|]

emptyBlogWidget :: Widget
emptyBlogWidget = [whamlet|
<h1>Empty!
<p>There could be a block post here. But since there is not.
<br>
<a .btn href=@{BlogAddR}>Click here to write a post
    |]

generateView :: Entity Blogpost -> Handler (Widget, Widget) -- ^ (Blogpost Widget, comments Widget)
generateView (Entity key val) = do
    comments <- runDB $ selectList [BlogcommentPost ==. key] [Asc BlogcommentTime]
    post     <- liftM (blogpostWidget val (length comments) False) isAdmin'
    return (post, blogcommentsWidget comments)

generatePreview :: Entity Blogpost -> Widget
generatePreview (Entity key val) = do
    comments <- liftHandlerT $ runDB $ count [BlogcommentPost ==. key]
    blogpostWidget val comments True =<< liftHandlerT isAdmin'

-- | A single post.
blogpostWidget :: Blogpost -- ^ Post to render.
               -> Int      -- ^ Number of comments.
               -> Bool     -- ^ Render as preview?
               -> Bool     -- ^ Include editing shortcuts?
               -> Widget
blogpostWidget post commentCount preview editing = let
    (title, poster, link) = ((,,) <$>
        blogpostTitle <*> blogpostPoster <*> BlogViewR . blogpostUrlpath) post
    in do
        [month, day, year] <- liftIO . liftM T.words $ formatTimeZoned "%b %d %Y" (blogpostTime post)
        $(widgetFile "blog-post")

blogcommentsWidget :: [Entity Blogcomment] -> Widget
blogcommentsWidget comments = let
    (parents, children) = part Nothing comments
    in if' (null parents) [whamlet|<i>No comments.|] $(widgetFile "blog-comments")
  where
    part mpid = L.partition ((==) mpid . blogcommentParent . entityVal)

    blogcommentsWidget' mpid children' = $(widgetFile "blog-comments") where
        (parents, children) = part mpid children'

commentForm :: BlogpostId          -- ^ main post
            -> Maybe BlogcommentId -- ^ Maybe parent comment
            -> Form  Blogcomment
commentForm pid mcid = renderBootstrap $ Blogcomment
    <$> pure pid
    <*> pure mcid
    <*> lift timeNow
    <*> lift maybeAuthId
    <*> areq textField "Name" Nothing
    <*> aopt urlField "Webpage" Nothing
    <*> areq textareaField "Message" Nothing

-- ** Tags

-- | Retrieve posts tagged under @tag@.
getBlogTagR :: Text -> Handler Html
getBlogTagR tag = do
    posts <- runDB $ rawSql query [toPersistValue $ "%\"s" <> tag <> "\"%"]
    defaultLayout $ do
        setTitle . toHtml $ "Posts under " <> tag
        renderBlog "" $ do
            [whamlet|\<i>Found #{length posts} posts.|]
            mapM_ generatePreview posts
  where
    query = "SELECT ?? FROM \"blogpost\" WHERE \"public\" = true AND \"tags\" SIMILAR TO ? ORDER BY \"time\""

renderTagcloud :: Text -> Widget
renderTagcloud current = do
    (posts, tagcloud) <- liftHandlerT . runDB . C.runResourceT $
        selectSource [BlogpostPublic ==. True] [Asc BlogpostTime]
        C.$$  CL.fold (\(x, y) p -> (sortMonth x p, addTags y $ entityVal p)) ([], [])
    $(widgetFile "blog-navigation")
  where
    addTags xs = L.union xs . blogpostTags
    getMonth   = formatTime defaultTimeLocale "%B %Y" . blogpostTime . entityVal

    sortMonth ys@((month, xs) : ys') x = let month' = getMonth x in
        if month == month'
            then (month , x : xs) : ys'
            else (month',    [x]) : ys
    sortMonth                     [] x = [(getMonth x, [x])]

tagField :: Field (HandlerT App IO) [Text]
tagField = let f = return . (Right :: [Text] -> Either Text [Text]) . T.words
               in checkMMap f T.unwords textField

-- * Content management

getBlogAddR :: Handler Html
getBlogAddR = do
    Entity _ user <- requireAuth
    (widget, encType) <- generateFormPost (blogpostForm False user Nothing) -- new post form
    renderPublish $ renderFormH $ myForm
        MsgBlogPublish encType BlogAddR
        widget (submitButton "Publish")
        FormMissing

postBlogAddR :: Handler Html
postBlogAddR = do
    Entity _ user <- requireAuth
    ((result, widget), encType) <- runFormPost (blogpostForm False user Nothing) -- new post form
    case result of
        FormSuccess post -> do
            void . runDB $ insert post
            setMessage . toHtml $ "Post " <> blogpostTitle post <> " published."
            redirect . BlogViewR $ blogpostUrlpath post
        _                -> renderPublish .  renderFormH $ myForm
            MsgBlogPublish encType BlogAddR
            widget (submitButton "Publish") result

getBlogEditR :: Text -> Handler Html
getBlogEditR path = do
    Entity _ user     <- requireAuth
    Entity _ val      <- runDB . getBy404 $ UniqueBlogpost path
    (widget, encType) <- generateFormPost (blogpostForm True user $ Just val)
    renderEdit val $ do
        [whamlet|<h1>Editing #{path}|]
        renderFormH $ myForm MsgBlogPublish encType (BlogEditR path)
                             widget (submitButton "Publish") FormMissing

postBlogEditR :: Text -> Handler Html
postBlogEditR path = do
    Entity _ user <- requireAuth
    Entity _ val  <- runDB $ getBy404 $ UniqueBlogpost path
    ((result, widget), encType) <- runFormPost $ blogpostForm True user (Just val)
    case result of
        FormSuccess post -> if blogpostUrlpath post /= path
            then invalidUrl
            else do
                updatePost post
                setMessage "Succesfully edited."
                redirect $ BlogViewR path
        FormFailure _ -> renderEdit val $ renderFormH $ myForm
                MsgBlogPublish encType (BlogEditR path)
                widget (submitButton "Publish") result
        FormMissing -> redirect $ BlogEditR path
  where
    invalidUrl = do
        setMessage "The url part should not be changed."
        redirect $ BlogEditR path
    updatePost post = runDB $ do
        Entity key _ <- getBy404 $ UniqueBlogpost path
        replace key post

-- * Forms
blogpostForm :: Bool -> User -> Maybe Blogpost -> Form Blogpost
blogpostForm editing poster mp = renderBootstrap $ mkBlogpost
    <$> lift (liftIO getCurrentTime)
    <*> areq textField     "Title"              (blogpostTitle    <$> mp)
    <*> areq urlpathField  "Unique URL part"    (blogpostUrlpath  <$> mp)
    <*> areq tagField      "Tags"               (blogpostTags     <$> mp)
    <*> areq boolField     "Public"             (blogpostPublic   <$> mp)
    <*> areq markdownField "Content (Markdown)" (blogpostMarkdown <$> mp)
  where
    urlpathField   = if' editing id (checkM validUrlpart) textField
    mkBlogpost time title url tags public md = Blogpost
        (maybe                  time blogpostTime   mp)
        (maybe (userUsername poster) blogpostPoster mp)
        title url tags public md
        (markdownToHtml md)
        (markdownToHtml $ getPreview md)
        (md == getPreview md)

    validUrlpart u = let
        toCheck = T.toLower u
        isLegal = isNothing $ T.find (\x -> not ( C.isAsciiLower x || C.isDigit x || x == '-' || x == '_')) toCheck
        in do
            dbentry <- runDB $ selectFirst [BlogpostUrlpath ==. toCheck] []
            let ret | isJust dbentry = Left ("Post with url already exists" :: Text)
                    | not isLegal    = Left "URL part contains illegal characters"
                    | otherwise      = Right toCheck
                in return ret

-- * Helpers

-- | Accumulating param fetching. @getParam param mydefault@:
--  * tries GET param
--  * tries cookie
--  * returns @mydefault@
getParam :: Text -> Text -> HandlerT master IO Text
getParam which fallback = fromGet
  where fromGet    = lookupGetParam which >>= maybe fromCookie return
        fromCookie = liftM (fromMaybe fallback) (lookupCookie which)

-- | Page based navigation in some content.
pageNav :: [Int]        -- ^ Available pages.
        -> Int          -- ^ Current page.
        -> Int          -- ^ Limit per page.
        -> Route master -- ^ Route for links.
        -> WidgetT master IO ()
pageNav    []       _     _     _ = mempty
pageNav   [_]       _     _     _ = mempty
pageNav pages current limit route = [whamlet|
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

replyButton :: Widget
replyButton = submitButton "reply"
