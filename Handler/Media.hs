module Handler.Media
    ( getMediaHomeR
    , getMediaContentR
    , getMediaServeR
    , postMediaAdminR
    , adminWidget
    ) where

import           Utils
import           Import
import           Configs
import           JSBrowser (browser)
import           Handler.Playlists (userPlaylistWidget)
import           Data.List (head, tail)
import           Data.Time.Clock (diffUTCTime, NominalDiffTime)

-- | Maximum time a file can be accessed via a temporary url.
maxTempUrlAlive :: NominalDiffTime
maxTempUrlAlive = 60 * 60 * 24

getMediaHomeR :: Handler RepHtml
getMediaHomeR = do
    ent <- requireAuth
    defaultLayout $ do
        setTitle "Media"
        navigation "Media"
        renderBrowsable ""
        layoutSplitH mempty (userPlaylistWidget ent)

getMediaContentR :: Text -> [Text] -> Handler RepHtml
getMediaContentR section fps = do
    ent <- requireAuth
    bare <- lookupGetParam "bare"
    case bare of
        Just  _ -> widgetBodyToRepHtml contents
        Nothing -> defaultLayout $ do
            setTitle "Media"
            navigation "Media"
            renderBrowsable section
            layoutSplitH (browser contents)
                         (userPlaylistWidget ent)
  where contents = sectionWidget section fps

-- | Generate content based on section and path.
sectionWidget :: Text -> [Text] -> Widget
sectionWidget s fps = join $ lift $ onSec' s (`sWContent` fps)

restrictedWidget :: Widget
restrictedWidget = [whamlet|
<div .center-box .small-box .text-center>
  <h1>Access restricted
  <p>
    You must be explicitly granted access to this part.<br/>
    Please, #
    <a.btn.btn-primary href="@{routeToLogin}">Login
    &nbsp;or #
    <a.btn.btn-info href="@{RegisterR}">Register
    .|]


-- * Playing & Downloading

-- | Downloading files.
-- /tmp does not require authentication, other kinds do.
getMediaServeR :: ServeType -- ^ kind of download
               -> Text      -- ^ file section
               -> [Text]    -- ^ file path
               -> Handler RepJson
getMediaServeR     _       _   [] = invalidArgs ["No file provided."]
getMediaServeR stype section path = case stype of
    ServeTemp           -> solveTemp         >>= send ""
    ServeAuto           -> solvePathWithAuth >>= send ""
    ServeForceDownload  -> solvePathWithAuth >>= send "application/force-download"
  where
    send ct fp = setHeader "Accept-Ranges" "bytes" >> sendFile ct fp

    solveTemp  = do
        Entity _ (DlTemp time _ target) <- runDB $ getBy404 $ UniqueDlTemp $ head path
        now <- timeNow
        denyIf (diffUTCTime now time > maxTempUrlAlive) "File not available."
        denyIf (target /= toPath (tail path)          ) "Malformed url."
        onSec section (`sFilePath` target)

    solvePathWithAuth = do
        uid <- requireAuthId
        fp  <- onSec section (flip sFilePath $ toPath path)
        t   <- timeNow
        _   <- runDB $ insert $ LogDownload uid t fp -- TODO: use a log file
        return fp


-- * Adminspace

-- | A widget for media administration functionality, to be embebbed in the
--   centralised admin page.
adminWidget :: Widget
adminWidget = do
    ((result, widget), encType) <- lift $ runFormPost adminForm
    renderFormH (submitButton "Execute actions")
                MsgMediaActions
                MediaAdminR
                result widget encType

-- | Admin operations in Media.
postMediaAdminR :: Handler RepHtml
postMediaAdminR = do
    ((result, _), _) <- runFormPost adminForm
    case result of
        FormSuccess (True,_) -> updateIndeces
        FormSuccess _        -> setMessage "No actions."
        _                    -> setMessage "Form failed!"
    redirect AdminR

adminForm :: Form (Bool, Bool)
adminForm = renderBootstrap $ (,)
     <$> areq boolField "Update every index" (Just False)
     <*> areq boolField "Not used" (Just False)
