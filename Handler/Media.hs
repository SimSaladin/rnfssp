module Handler.Media
    ( getMediaHomeR
    , getMediaContentR
    , getMediaServeR
    , getMediaSearchAllR
    , getMediaSearchR
    , postMediaAdminR
    , adminWidget
    ) where

import           Utils
import           Import
import           Configs
import           JSBrowser (browser)
import           Handler.Playlists (userPlaylistWidget)
import qualified Data.Text as T
import           Data.List (head, tail)
import           Data.Time.Clock (diffUTCTime, NominalDiffTime)
import qualified Data.Map as Map

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
        layoutSplitH renderSearchAll $ userPlaylistWidget ent

renderMaybeBare :: Widget -> Widget -> Handler RepHtml
renderMaybeBare bareContents nonBareContents = do
    mq <- lookupGetParam "bare"
    case mq of
        Just _  -> widgetBodyToRepHtml bareContents
        Nothing -> defaultLayout $ do
            navigation "Media"
            nonBareContents

getMediaContentR :: Text -> [Text] -> Handler RepHtml
getMediaContentR section fps = do
    ent <- requireAuth
    renderMaybeBare content $ do
        setTitle "Media"
        renderBrowsable section
        renderSearch section
        layoutSplitH (browser content [".input-search + button"])
                     (userPlaylistWidget ent)
  where content = sectionWidget section fps

getMediaSearchAllR :: Handler RepHtml
getMediaSearchAllR = do
    ent <- requireAuth
    mq  <- lookupGetParam "q"
    let f section hres = hres >>= \res -> return [whamlet|
<h3>#{section}
^{res}
|] 
    let doSearch q
          | T.null q  = do
            let msg = "Search must be at least one character!" :: Html
            renderMaybeBare [whamlet|<i>#{msg}|] $ liftHandlerT $ do
                setMessage msg
                redirect MediaHomeR

          | otherwise = do
            results' <- onSecs' $ flip sWSearch q
            results  <- liftM mconcat $ sequence $ Map.elems $ Map.mapWithKey f results'
            renderMaybeBare results $ do
                setTitle $ toHtml $ "Results for: " `mappend` q
                renderBrowsable ""
                renderSearchAll
                layoutSplitH (browser results [".input-search + button"])
                             (userPlaylistWidget ent)
        in maybe (redirect $ MediaHomeR) doSearch mq

-- | search in section @section@ using query string from the @q@ get parameter.
getMediaSearchR :: Section -> Handler RepHtml
getMediaSearchR section = do
    ent <- requireAuth
    mq  <- lookupGetParam "q"
    let doSearch q
          | T.null q  = do
            let msg = "Search must be at least one character!" :: Html
            renderMaybeBare [whamlet|<i>#{msg}|] $ liftHandlerT $ do
                setMessage msg
                redirect $ MediaContentR section []

          | otherwise = do
            results <- onSec' section $ flip sWSearch q
            renderMaybeBare results $ do
                setTitle $ toHtml $ "Results for: " `mappend` q
                renderBrowsable section
                renderSearch section
                layoutSplitH (browser results [".input-search + button"])
                             (userPlaylistWidget ent)
    maybe (redirect $ MediaContentR section []) doSearch mq

-- | Generate content based on section and path.
sectionWidget :: Text -> [Text] -> Widget
sectionWidget s fps = join $ liftHandlerT $ onSec' s (`sWContent` fps)

renderSearch :: Section -> Widget
renderSearch section = [whamlet|$newline never
<form .bare .text-center .standout action=@{MediaSearchR section} type=get>
  <input .input-search type="search" name="q" placeholder="In #{section}" autofocus pattern="..*" required>
  <button>Go
|]

renderSearchAll :: Widget
renderSearchAll = [whamlet|$newline never
<form .bare .text-center .standout action=@{MediaSearchAllR} type=get>
  <input .input-search type="search" name="q" placeholder="Search Media" autofocus pattern="..*" required>
  <button>Go
|]

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
    send ct fp = addHeader "Accept-Ranges" "bytes" >> sendFile ct fp

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

-- * Sections



-- * Adminspace

-- | A widget for media administration functionality, to be embebbed in the
--   centralised admin page.
adminWidget :: Widget
adminWidget = do
    ((result, widget), encType) <- liftHandlerT $ runFormPost adminForm
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
