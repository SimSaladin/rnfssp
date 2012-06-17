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
import Data.Maybe (isJust, isNothing)
import qualified Data.Text as T
import Data.Time (getCurrentTime, formatTime, UTCTime)
import System.Locale (defaultTimeLocale)
import Yesod.Form.Nic (nicHtmlField)
import Text.Blaze.Html.Renderer.Text

titleRender :: [String] -> Widget
titleRender texts = setTitle $ toHtml $ foldl (++) "blog >>= " texts

getBlogOverviewR :: Handler RepHtml
getBlogOverviewR = do
    posts <- runDB $ selectList ([] :: [Filter Blogpost]) []
    ma <- maybeAuth
    let previews = map (\x -> blogpostWidget (entityVal x) True) posts
        isAdmin = case ma of
            Nothing -> False
            Just ent -> userAdmin $ entityVal ent
    (widget, encType) <- generateFormPost newpostForm
    defaultLayout $ do
        titleRender ["blog"]
        $(widgetFile "blog-home")

postBlogOverviewR :: Handler RepHtml
postBlogOverviewR = do
    ((result,_),_) <- runFormPost newpostForm
    case result of
        FormSuccess p -> do
            pid <- runDB $ insert p
            setMessage "success: added"
            redirect $ BlogViewR $ blogpostUrltitle p
        FormFailure msg -> do
            setMessage $ toHtml $ T.intercalate "\n" msg
            redirect $ BlogOverviewR
        _ -> redirect $ BlogOverviewR

getBlogEditR :: Text -> Handler RepHtml
getBlogEditR utitle = do
    defaultLayout $ do
        titleRender ["<teh title>", " [edit]"]
        $(widgetFile "blog-edit")

postBlogEditR :: Text -> Handler RepHtml
postBlogEditR utitle = redirect $ BlogEditR utitle

getBlogViewR :: Text -> Handler RepHtml
getBlogViewR utitle = do
    (post, comments) <- runDB $ do
        p <- getBy404 $ UniqueBlogpost utitle
        cmnts <- selectList
                    [BlogCommentPost ==. entityKey p]
                    [Asc BlogCommentTime]
        return (p, cmnts)
    let postWidget = blogpostWidget (entityVal post) False
    defaultLayout $ do
    titleRender ["<teh title>"]
    $(widgetFile "blog-view")

postBlogViewR :: Text -> Handler RepHtml
postBlogViewR utitle = do
    redirect $ BlogViewR utitle

newpostForm :: Html -> MForm App App (FormResult Blogpost, Widget)
newpostForm extra = do
    (titleRes, titleView) <- mreq textField "Title" Nothing
    (urlpartRes, urlpartView) <- mreq urlpartField "with url" Nothing
    (contentRes, contentView) <- mreq nicHtmlField "Content" Nothing
    time <- liftIO getCurrentTime
    let blogpostRes = Blogpost
            <$> pure time
            <*> pure "bps"
            <*> titleRes <*> urlpartRes <*> contentRes
        widget = do
            toWidget [lucius|##{fvId contentView} { width:54em; }
|]
            [whamlet|#{extra}
<label for=#{fvId titleView}>^{fvLabel titleView}
^{fvInput titleView}                      #
<label for=#{fvId urlpartView}>^{fvLabel urlpartView}
^{fvInput urlpartView}
<br>
^{fvInput contentView}
    <input type=submit value="add new post">
|]
    return (blogpostRes, widget)
  where
    urlpartField = checkM validUrlpart textField

    validUrlpart u = do
        dbentry <- runDB $ selectFirst [BlogpostPoster ==. toCheck] []
        if isLegal && isNothing dbentry
            then return $ Right toCheck
            else return $ Left ("Url part is not valid"::Text)
      where
        toCheck = T.toLower u
        isLegal = isNothing $ T.find (\x -> not $ (
                      C.isAsciiLower x || C.isDigit x || x == '-' || x == '_'
                      )) toCheck

blogpostWidget :: Blogpost -> Bool -> Widget
blogpostWidget post preview = do
    -- TODO; get the first paragraph or so from the posts in preview mode
    toWidget [whamlet|
<div.page-element.blogpost>
    ^{time}
    <h2>#{blogpostTitle post}
    $if preview
        <p>^{toWidget cnt}
    $else
        <p>^{toWidget cnt}
    <cite>
        Posted by #{blogpostPoster post} under ... | #
        <a href=@{BlogViewR $ blogpostUrltitle post}>Link #
        | Comments
    |]
  where
    time = formatTimeBox $ blogpostTime post
    cnt = blogpostContent post

formatTimeBox :: UTCTime -> Widget
formatTimeBox t = toWidget [hamlet|
<div.date-box>
    #{month}
    <span>#{day}
|]
  where
    locale = defaultTimeLocale
    fmt = \x -> formatTime locale x t
    month = fmt "%b"
    day = fmt "%d"

