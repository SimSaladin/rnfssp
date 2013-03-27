module Handler.Blog
    ( getBlogOverviewR
    , postBlogOverviewR
    , getBlogViewR
    , postBlogViewR
    , getBlogEditR
    , postBlogEditR
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

-- * Helpers

overview :: ((FormResult a, Widget), Enctype) -> Handler RepHtml
overview ((result, widget), encType) = do
    perPage    <- liftM (read . T.unpack) (getParam "results" "20")
    pageNumber <- liftM (read . T.unpack) (getParam "page" "1")
    canPost    <- isAdmin'
    (pcount, posts, ccounts) <- runDB $ do
        pcount <- count ([] :: [Filter Blogpost])
        posts  <- selectList [] [ Desc BlogpostTime
                                , LimitTo perPage
                                , OffsetBy $ (pageNumber - 1) * perPage
                                ]
        ccounts <- mapM (count . genFilter) posts
        return (pcount, posts, ccounts)
    let genWidget ent cn = blogpostWidget (entityVal ent) cn True canPost
        previews = zipWith genWidget posts ccounts
        -- TODO: calculate omitted pages and add navigation
    defaultLayout $ do
        titleRender ["blog"]
        navigation "Blog"
        renderBlog $ $(widgetFile "blog-overview")
  where
    genFilter x = [BlogcommentPost ==. entityKey x]

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
        titleRender ["blog", blogpostTitle val]
        navigation "Blog"
        renderBlog $ $(widgetFile "blog-view")

edit :: ((FormResult a, Widget), Enctype) -> Text -> Handler RepHtml
edit ((result, widget), encType) path = defaultLayout $ do
    titleRender ["blog/edit"]
    renderBlog $ $(widgetFile "blog-edit")

getParam :: Text -> Text -> Handler Text
getParam which fallback = fromGet
  where fromGet = lookupGetParam which >>= maybe (fromCookie) return
        fromCookie = lookupCookie which >>= return . maybe fallback id

-- * Widgets

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
blogpostWidget post commentCount preview editing = $(widgetFile "blogpost")
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


-- * Forms

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
