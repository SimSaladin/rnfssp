module Handler.Posts
    ( getPostNewestR
    , getPostsViewR, postPostsViewR
    , getPostsEditR, postPostsEditR
    , getPostsAddR,  postPostsAddR
    , getPostsTagR
    , activePosts
    ) where

import           Import
import qualified Data.List          as L
import qualified Data.Conduit       as C
import qualified Data.Conduit.List  as CL
import qualified Data.Text          as T
import           Database.Persist.Sql (rawSql)
import qualified Database.Esqueleto as E
import           Yesod.Markdown (Markdown(..), markdownField, markdownToHtml)

-- * Posts

-- ** Handlers

-- | Home shows the newest post.
getPostNewestR :: Handler Html
getPostNewestR = do
    now <- timeNow
    maybeNewest <- runDB (selectFirst [PostExpires <=. now] [Desc PostAdded])
    maybe noPosts (defaultLayout . viewPost False) maybeNewest

-- | View a post specified in the url.
getPostsViewR :: PostId -> Handler Html
getPostsViewR pid = do
    post <- runDB $ get404 pid
    defaultLayout $ do
        setTitle . toHtml $ postTitle post
        viewPost False (Entity pid post)

-- | Post action in a view is for posting comments.
postPostsViewR :: PostId -> Handler Html
postPostsViewR = getPostsViewR

viewPost :: Bool -> Entity Post -> Widget
viewPost preview (Entity pid post) = do
    [month, day, year] <- liftIO . liftM T.words $ formatTimeZoned "%b %d %Y" (postAdded post)

--     commentCounts <- liftHandlerT . runDB $ do
--         mapM (\Entity pid _ -> count [PostCommentPost ==. pid]) posts

    let comments = postComments pid
        result   = FormMissing

    $(widgetFile "post-view")

getPostsAddR, postPostsAddR :: Handler Html
getPostsAddR  = publishing Nothing
postPostsAddR = publishing Nothing

getPostsEditR, postPostsEditR :: PostId -> Handler Html
getPostsEditR = publishing . Just
postPostsEditR = publishing . Just

publishing :: Maybe PostId -> Handler Html
publishing mpid = do
    Entity _ user <- requireAuth
    mpost <- maybe (return Nothing) (liftM Just . runDB . get404) mpid

    ((result, widget), encType) <- runFormPost $ postForm user mpost

    let formSettings = myFormSettings
                     { mfTitle = MsgPostPublish
                     , mfRoute = maybe PostsAddR PostsEditR mpid
                     }
    case result of
        FormSuccess post -> do
            pid <- maybe (newPost post) (editPost post) mpid
            redirect $ PostsViewR pid

        _ -> defaultLayout $ do
            setTitle "Publishing"
            $(widgetFile "post-publishing")
  where
    newPost post = do
        pid <- runDB $ insert post
        setMessage . toHtml $ "Post " <> postTitle post <> " added"
        return pid

    editPost post pid = do
        runDB $ replace pid post
        setMessage "Succesfully edited."
        return pid

-- | Retrieve posts tagged under @tag@.
getPostsTagR :: Text -> Handler Html
getPostsTagR tag = do
    posts <- runDB $ rawSql query [toPersistValue $ "%\"s" <> tag <> "\"%"]

    defaultLayout $ do
        setTitle . toHtml $ "Posts under " <> tag
        [whamlet|\<i>Found #{length posts} posts.|]
        mapM_ (viewPost True) posts
  where
    query = "SELECT ?? FROM \"post\" WHERE \"tags\" SIMILAR TO ? ORDER BY \"added\""

noPosts :: Handler Html
noPosts = defaultLayout $ do
    setTitle "Posts: No posts."
    emptyPostsWidget

-- ** Widgets

-- | Render a preview of the most recent post.
activePosts :: Widget
activePosts = do
    now <- timeNow
    posts <- liftHandlerT . runDB $ selectList [PostExpires >=. now] [Desc PostAdded]
    case posts of
        [] -> emptyPostsWidget
        _  -> mapM_ (viewPost True) posts

emptyPostsWidget :: Widget
emptyPostsWidget = [whamlet|
<p>The posts section is empty.
<br>
<a .btn .center href=@{PostsAddR}>Click here to write a post
    |]

renderTagcloud :: Text -> Widget
renderTagcloud current = do
    (posts, tagcloud) <- liftHandlerT . runDB . C.runResourceT $
        selectSource [] [Asc PostAdded]
        C.$$  CL.fold (\(x, y) p -> (sortMonth x p, addTags y $ entityVal p)) ([], [])
    $(widgetFile "post-navigation")
  where
    addTags xs = L.union xs . postTags
    getMonth   = formatTime defaultTimeLocale "%B %Y" . postAdded . entityVal

    sortMonth ys@((month, xs) : ys') x = let month' = getMonth x in
        if month == month'
            then (month , x : xs) : ys'
            else (month',    [x]) : ys
    sortMonth                     [] x = [(getMonth x, [x])]

-- ** Forms

postForm :: User -> Maybe Post -> Form Post
postForm poster mp = renderBootstrap $ Post
    <$> lift timeNow
    <*> areq utctimeField  "Expires"            (postExpires  <$> mp)
    <*> areq textField     "Title"              (postTitle    <$> mp)
    <*> areq textField     "Author"             (postAuthor   <$> mp)
    <*> areq tagField      "Tags"               (postTags     <$> mp)
    <*> areq contentField  "Content (Markdown)" (postContent  <$> mp)

tagField :: Field (HandlerT App IO) [Text]
tagField = let f = return . (Right :: [Text] -> Either Text [Text]) . T.words
               in checkMMap f T.unwords textField

utctimeField :: Field Handler UTCTime
utctimeField = checkMMap toTime utctDay dayField
    where
        toTime :: Day -> Handler (Either Text UTCTime)
        toTime day = return $ Right $ UTCTime day 0

-- ** Helpers

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

-- * Comments

-- | Render comments and add form for a post.
postComments :: PostId -> Widget
postComments pid = do

    -- TODO parent comment from GET param or something?
    ((result, widget), encType) <- liftHandlerT $ runFormPost $ commentForm Nothing

    -- handle possible form
    liftHandlerT $ case result of
        FormSuccess comment -> do
            void . runDB $ insert comment >>= insert . PostComment pid
            setMessage "Comment sent."
            redirect $ PostsViewR pid
        _  -> return ()

    -- fetch all comments for post
    comments <- liftHandlerT $ runDB $ E.select $ E.from $ \(postComment `E.InnerJoin` comment) -> do
        E.on (postComment E.^. PostCommentComment E.==. comment E.^. CommentId)
        E.where_ (postComment E.^. PostCommentPost E.==. E.val pid)
        return comment

    let (parents, children) = part Nothing comments

    -- recursive!
    case parents of
        [] -> [whamlet|<i>No comments.|]
        _  -> $(widgetFile "post-comments")

    -- add form
    renderForm $ myFormSettings
          { mfTitle   = MsgPostComment
          , mfEnctype = encType
          , mfRoute   = PostsViewR pid
          , mfFields  = widget
          , mfResult  = result
          , mfActions = replyButton
          }
  where
    part mpid = L.partition ((==) mpid . commentParent . entityVal)

    commentsWidget' mpid children' =
        let (parents, children) = part mpid children'
            in $(widgetFile "post-comments")

commentForm :: Maybe CommentId -- ^ Maybe parent comment
            -> Form Comment
commentForm mParentId = renderBootstrap $ Comment
    <$> pure mParentId
    <*> lift timeNow
    <*> lift maybeAuthId
    <*> areq textField "Name" Nothing
    <*> areq contentField "Comment" Nothing

contentField :: Field Handler UserContent
contentField = checkMMap toUserContent userContentMarkdown markdownField
    where
        toUserContent :: Markdown -> Handler (Either Text UserContent)
        toUserContent md
            | md == getPreview md = return $ Right $ UserContent md (markdownToHtml md) Nothing
            | otherwise           = return $ Right $ UserContent md (markdownToHtml md) (Just $ markdownToHtml $ getPreview md)
