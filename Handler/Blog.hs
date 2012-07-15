module Handler.Blog
    ( getBlogOverviewR
    , postBlogOverviewR
    , getBlogViewR
    , postBlogViewR
    , getBlogEditR
    , postBlogEditR
    ) where

import Import
import qualified Data.Char as C
import Data.Maybe
import qualified Data.Text as T
import Data.Time (getCurrentTime, formatTime)
import System.Locale (defaultTimeLocale)
import Yesod.Form.Nic (nicHtmlField)

getBlogOverviewR :: Handler RepHtml
getBlogOverviewR = do
    canPost <- isAdmin
    previews <- previewWidgets
    ((result, widget), encType) <- runFormPost newpostForm
    defaultLayout $ do
        titleRender []
        $(widgetFile "blog-home")

postBlogOverviewR :: Handler RepHtml
postBlogOverviewR = do
    ((result,widget),encType) <- runFormPost newpostForm
    case result of
        FormSuccess p -> do
            pid <- runDB $ insert p
            redirect $ BlogViewR $ blogpostUrltitle p
        _ -> do
            canPost <- isAdmin
            previews <- previewWidgets
            defaultLayout $ do
                titleRender []
                $(widgetFile "blog-home")

getBlogViewR :: Text -> Handler RepHtml
getBlogViewR utitle = do
    ((result, replyw), encType, postw, commentsw) <- postAndReplies utitle
    defaultLayout $ do
        titleRender ["<teh title>"]
        $(widgetFile "blog-view")

postBlogViewR :: Text -> Handler RepHtml
postBlogViewR utitle = do
    ((result, replyw), encType, postw, commentsw) <- postAndReplies utitle
    case result of
        FormSuccess r -> do
            rId <- runDB $ insert r
            setMessage "Successfully replied"
        _ -> return ()
    defaultLayout $ do
        titleRender ["<teh title>"]
        $(widgetFile "blog-view")

getBlogEditR :: Text -> Handler RepHtml
getBlogEditR utitle = do
    defaultLayout $ do
        titleRender ["<teh title>", " [edit]"]
        $(widgetFile "blog-edit")

postBlogEditR :: Text -> Handler RepHtml
postBlogEditR utitle = redirect $ BlogEditR utitle

isAdmin :: Handler Bool
isAdmin = maybeAuth >>= \ma -> return $ case ma of
    Nothing -> False
    Just ent -> userAdmin $ entityVal ent

titleRender :: [String] -> Widget
titleRender texts = setTitle $ toHtml $ foldr (++) "blog" texts

postAndReplies :: Text
               -> Handler ((FormResult BlogComment, Widget), Enctype, Widget, [Widget])
postAndReplies utitle = do
    (p, cs) <- runDB $ do
        p <- getBy404 $ UniqueBlogpost utitle
        cmnts <- selectList
                    [BlogCommentPost ==. entityKey p]
                    [Desc BlogCommentTime]
        return (p, cmnts)
    let pw = blogpostWidget (entityVal p) (-1) False
        cw = map (\x -> blogcommentWidget x cs) cs
    ((rr, rw), re) <- runFormPost $ replyForm (entityKey p) Nothing
    return ((rr, rw), re, pw, cw)

previewWidgets :: Handler [Widget]
previewWidgets = do
    (posts, ccount) <- runDB $ do
        posts <- selectList ([] :: [Filter Blogpost]) [Asc BlogpostTime]
        comments <- mapM count (map getFilter posts)
        return (posts, comments)
    let widgets = zipWith toWidget posts ccount
        in return widgets
  where
    toWidget p cc = blogpostWidget (entityVal p) cc True
    getFilter x = [BlogCommentPost ==. entityKey x]

blogcommentWidget :: Entity BlogComment -> [Entity BlogComment] -> Widget
blogcommentWidget c cs = do
    $(widgetFile "blogcomment")
  where
    children = filter (\x -> Just (entityKey c) == (blogCommentParent $ entityVal x)) cs
    parent = entityVal c

blogpostWidget :: Blogpost -> Int -> Bool -> Widget
blogpostWidget post commentCount preview = do
    -- TODO; get the first paragraph or so from the posts in preview mode
    -- COMMENT: really needed?
    $(widgetFile "blogpost")
  where
    fmt = \x -> formatTime defaultTimeLocale x (blogpostTime post)
    month = fmt "%b"
    day = fmt "%d"
    cnt = blogpostContent post

newpostForm :: Html -> MForm App App (FormResult Blogpost, Widget)
newpostForm extra = do
    time <- liftIO getCurrentTime
    (titleRes, titleView) <- mreq textField "Title" Nothing
    (urlpartRes, urlpartView) <- mreq urlpartField "Title (in URL)" Nothing
    (contentRes, contentView) <- mreq nicHtmlField "Content (HTML)" Nothing
    let blogpostRes = Blogpost <$> pure time
                               <*> pure "bps"
                               <*> titleRes
                               <*> urlpartRes
                               <*> contentRes
        widget = $(widgetFile "blog-form-newpost")
        in return (blogpostRes, widget)
  where
    urlpartField = checkM validUrlpart textField
    validUrlpart u = do
        dbentry <- runDB $ selectFirst [BlogpostPoster ==. toCheck] []
        let ret
                | isJust dbentry = Left ("Post with url `` already exists"::Text)
                | not isLegal = Left ("URL part contains illegal characters"::Text)
                | otherwise = Right toCheck
            in return ret
      where
        toCheck = T.toLower u
        isLegal = isNothing $ T.find (\x -> not $ (
                      C.isAsciiLower x || C.isDigit x || x == '-' || x == '_'
                      )) toCheck

replyForm :: BlogpostId -> Maybe BlogCommentId -> Form BlogComment
replyForm pid mcid = renderBootstrap $ BlogComment
    <$> pure pid
    <*> pure mcid
    <*> aformM (liftIO getCurrentTime)
    <*> pure Nothing --TODO user creds checking
    <*> areq textField "Name" Nothing
    <*> aopt urlField "Webpage" Nothing
    <*> areq textareaField "Message" Nothing
