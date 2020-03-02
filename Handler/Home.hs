{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Handler.Posts
import Language.Haskell.TH ( Exp(..) )

import qualified Data.Text as T
import qualified Haikubot.Plugins.Runot as H

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")
        $(fayFile' (ConE 'StaticR) "Home")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

getRegisterR :: Handler Html
getRegisterR = undefined

-- | Documentation for 'name'
postRegisterR :: Handler Html
postRegisterR = error "undefined: `name' in /home/sim/the-dashboard/Handler/Home.hs"

-- | Documentation for 'name'
getAdminR :: Handler Html
getAdminR = error "undefined: `name' in /home/sim/the-dashboard/Handler/Home.hs"

-- | Documentation for 'name'
postAdminR :: Handler Html
postAdminR = error "undefined: `name' in /home/sim/the-dashboard/Handler/Home.hs"

dashboardWidget :: Widget
dashboardWidget = do
    now <- liftIO (formatTimeZoned' =<< getCurrentTime)
    $(widgetFile "dashboard")

linksWidget :: Widget
linksWidget = do
    links <- liftHandlerT $ runDB $ selectList [] [Desc LinkAdded]

    $(widgetFile "links")

getLinkAddR :: Handler Html
getLinkAddR = defaultLayout linksAddWidget

postLinkAddR :: Handler Html
postLinkAddR = getLinkAddR

linksAddWidget :: Widget
linksAddWidget = do
    ((result, widget), enctype) <- liftHandlerT $ runFormPost linkForm

    case result of
        FormSuccess link -> liftHandlerT $ do
            runDB $ insert link
            redirect HomeR

        _ -> renderForm myFormSettings
            { mfRoute = LinkAddR
            , mfResult = result
            , mfFields = widget
            , mfEnctype = enctype
            }

linkForm :: Form Link
linkForm = renderBootstrap $ Link
    <$> lift timeNow
    <*> areq textField "Author" Nothing
    <*> areq urlField "Address" Nothing
    <*> aopt textField "Description" Nothing

getStyleTestR :: Handler Html
getStyleTestR = defaultLayout
    $(widgetFile "style-test")

haikuQuoteWidget :: Widget
haikuQuoteWidget = do
    txts <- liftIO . (H.getRandomHaiku >=> H.format) . extraHaikuFile =<< liftHandlerT getExtra
    [whamlet|
<blockquote #haiku-quote>
    $forall txt <- txts
        $forall sae <- haikuf txt
            <p>#{sae}
|]
  where
    haikuf :: Text -> [Text]
    haikuf = map T.strip
           . concatMap (T.splitOn "--")
           . concatMap (T.splitOn "/")
           . concatMap (T.splitOn "//")
           . T.splitOn ";"
    
shoutbox :: Widget
shoutbox = do
    shouts <- liftHandlerT $ runDB $ selectList [] [Desc ShoutAdded, LimitTo 20]
    times <- liftIO $ mapM (formatTimeZoned' . shoutAdded . entityVal) shouts

    let xs = zip times $ map (shoutContent . entityVal) shouts
        in $(widgetFile "shoutbox")

postShoutR :: Handler Html
postShoutR = do
    newShout <- runInputPost $ ireq textField "shout"
    now <- timeNow
    _ <- runDB $ insert $ Shout now newShout
    redirect HomeR

