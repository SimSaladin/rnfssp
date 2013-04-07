module Handler.Blog
    ( getBlogOverviewR
    , postBlogOverviewR
    , getBlogViewR
    , postBlogViewR
    , getBlogEditR
    , postBlogEditR
    , newestPost
    ) where

import           Utils
import           Import
import qualified Data.List as L
import qualified Data.Char as C
import           Data.Maybe
import qualified Data.Text as T
import           Data.Time (formatTime)
import           System.Locale (defaultTimeLocale)
import           Yesod.Markdown (markdownField, markdownToHtml)
import Chat

-- * Handlers
-- ** Overview

getBlogOverviewR :: Handler RepHtml
getBlogOverviewR = overview =<< runFormPost (blogpostForm Nothing)

postBlogOverviewR :: Handler RepHtml
postBlogOverviewR = do
    form <- runFormPost $ blogpostForm Nothing
    case fst $ fst form of
        FormSuccess p -> do
            _ <- runDB $ insert p
            setMessage "New blog post published!"
            redirect $ BlogViewR $ blogpostUrlpath p
        _ -> overview form

overview :: ((FormResult a, Widget), Enctype) -> Handler RepHtml
overview ((result, widget), encType) = do
    perPage    <- liftM (read . T.unpack) (getParam "per_page" "5")
    page       <- liftM (read . T.unpack) (getParam "page" "1")
    canPost    <- isAdmin'
    (pcount, posts, ccounts) <- runDB $ do
        pcount <- count ([] :: [Filter Blogpost])
        posts  <- selectList [] [ Desc BlogpostTime
                                , LimitTo perPage
                                , OffsetBy $ (page - 1) * perPage
                                ]
        ccounts <- mapM (count . genFilter) posts
        return (pcount, posts, ccounts)
    let genWidget ent cn = blogpostWidget (entityVal ent) cn True canPost
        previews         = zipWith genWidget posts ccounts
        pages            = reverse [1 .. (ceiling $ (fromIntegral pcount :: Double) / fromIntegral perPage :: Int)]
        pageNav'         = pageNav pages page perPage BlogOverviewR
        -- TODO: calculate omitted pages and add navigation
    defaultLayout $ do
        setTitle "SS Blog"
        navigation "Blog"
        renderBlog $ $(widgetFile "blog-overview")
  where
    genFilter x = [BlogcommentPost ==. entityKey x]

-- ** View

getBlogViewR :: Text -> Handler RepHtml
getBlogViewR path = do
    ent <- runDB $ getBy404 $ UniqueBlogpost path
    form <- runFormPost $ commentForm (entityKey ent) Nothing
    view form ent

postBlogViewR :: Text -> Handler RepHtml
postBlogViewR path = do
    ent <- runDB $ getBy404 $ UniqueBlogpost path
    form <- runFormPost (commentForm (entityKey ent) Nothing)
    case fst $ fst form of
        FormSuccess comment -> do
            _ <- runDB $ insert comment
            setMessage "Comment sent."
            redirect $ BlogViewR path
        _  -> view form ent

view :: ((FormResult a, Widget), Enctype) -- ^ commentForm widget
     -> Entity Blogpost                   -- ^ post to view
     -> Handler RepHtml
view ((result, formWidget), encType) (Entity key val) = do
    (mprev, mnext) <- runDB $ do
        a <- selectFirst [BlogpostId <. key] [Desc BlogpostId]
        b <- selectFirst [BlogpostId >. key] [Desc BlogpostId]
        return (a, b)
    comments <- runDB $ selectList [BlogcommentPost ==. key] [Desc BlogcommentTime]
    post <- liftM (blogpostWidget val (length comments) False) isAdmin'
    let commentsWidget = blogcommentsWidget Nothing comments
        route          = BlogViewR $ blogpostUrlpath val
    defaultLayout $ do
        setTitle $ toHtml (blogpostTitle val) `mappend` " - SS Blog"
        navigation "Blog"
        renderBlog $ $(widgetFile "blog-view")


-- ** Edit

getBlogEditR :: Text -> Handler RepHtml
getBlogEditR path = do
    _ <- requireAuth
    Entity _ val <- runDB $ getBy404 $ UniqueBlogpost path
    flip edit path =<< runFormPost (blogpostForm (Just val))

postBlogEditR :: Text -> Handler RepHtml
postBlogEditR path = do
    form <- runFormPost (blogpostForm Nothing)
    case fst $ fst form of
      FormSuccess post -> do
          runDB $ do
              Entity key _ <- getBy404 $ UniqueBlogpost path
              update key [ BlogpostMarkdown =. blogpostMarkdown post
                         , BlogpostRendered =. markdownToHtml (blogpostMarkdown post)
                         ]
          setMessage "Succesfully edited"
          redirect $ BlogViewR path
      _ -> edit form path

edit :: ((FormResult a, Widget), Enctype) -> Text -> Handler RepHtml
edit ((result, widget), encType) path = defaultLayout $ do
    setTitle $ "editing: " `mappend` toHtml path `mappend` " - SS Blog"
    renderBlog $ $(widgetFile "blog-edit")

-- * Helpers

getParam :: Text -> Text -> Handler Text
getParam which fallback = fromGet
  where fromGet = lookupGetParam which >>= maybe (fromCookie) return
        fromCookie = lookupCookie which >>= return . maybe fallback id

newestPost :: Widget
newestPost = do
    mpost <- lift $ runDB $ selectFirst [] [Desc BlogpostTime]
    maybe mempty constructPost mpost
    where
    constructPost (Entity key val) = do
        commentCount <- lift $ runDB $ count [BlogcommentPost ==. key]
        blogpostWidget val commentCount True False

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


-- ** Widgets

-- | Wrap blog content with chat and tags.
renderBlog :: Widget -> Widget
renderBlog content = do
    $(widgetFile "blog-wrapper")

-- | A single post.
-- TODO; Get the first paragraph or so from the posts in preview mode.
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
            rendered = blogpostRendered post

blogcommentsWidget :: Maybe BlogcommentId -> [Entity Blogcomment] -> Widget
blogcommentsWidget parent comments = $(widgetFile "blog-comments")
  where (parents, children) = L.partition ((==) parent . blogcommentParent . entityVal) comments

tags :: Widget
tags = do
    [whamlet|
<h5>Selaa
<h6>Päivämäärä
<ul>
    <li>
        <i>Tag

<h6>Tageittain
    |]


-- ** Forms

blogpostForm :: Maybe Blogpost -> Html -> MForm App App (FormResult Blogpost, Widget)
blogpostForm mp extra = do
    time <- lift timeNow
    (resTitle, viewTitle) <- mreq textField "Title" (blogpostTitle <$> mp)
    (resURL,   viewURL  ) <- mreq urlpathField "Unique URL part" (blogpostUrlpath <$> mp) -- remove?
    (resMD,    viewMD   ) <- mreq markdownField "Content :: Markdown" (blogpostMarkdown <$> mp)
    let res = Blogpost <$> pure time
                       <*> pure "bps" -- XXX: ...
                       <*> resTitle
                       <*> resURL
                       <*> resMD
                       <*> (markdownToHtml <$> resMD)
        widget = $(widgetFile "blog-form-newpost")
        in return (res, widget)
  where
    urlpathField = checkM validUrlpart textField
    validUrlpart u = do
        dbentry <- runDB $ selectFirst [BlogpostPoster ==. toCheck] []
        let ret
                | isJust dbentry = Left ("Post with url `` already exists"::Text)
                | not isLegal = Left ("URL part contains illegal characters"::Text)
                | otherwise = Right toCheck
            in return ret
      where
        toCheck = T.toLower u
        isLegal = isNothing $ T.find (\x -> not ( C.isAsciiLower x || C.isDigit x || x == '-' || x == '_')) toCheck

commentForm :: BlogpostId          -- ^ main post
            -> Maybe BlogcommentId -- ^ Maybe parent comment
            -> Form Blogcomment
commentForm pid mcid = renderYaml $ Blogcomment
    <$> pure pid
    <*> pure mcid
    <*> aformM timeNow
    <*> pure Nothing --TODO user creds checking
    <*> areq textField "Name" Nothing
    <*> aopt urlField "Webpage" Nothing
    <*> areq textareaField "Message" Nothing
