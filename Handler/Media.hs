{-# LANGUAGE RankNTypes #-}
module Handler.Media
    ( getMediaHomeR
    , getMediaContentR
    , getMediaServeR
    , getMediaSearchAllR
    , getMediaSearchR
    , postMediaAdminR
    , adminWidget
    , mediaRecent, mediaRecentDl
    ) where

import           Utils
import           Import
import qualified Data.Conduit.List as CL
import           Data.Conduit
import qualified Data.Text as T
import           Data.List (head, tail, last)
import           Data.Time.Clock (diffUTCTime, NominalDiffTime)
import qualified Data.Map as Map
import qualified System.FilePath as FP
import           Handler.Playlists (userPlaylistWidget)
import           JSBrowser (browser)
import           Sections

-- | Maximum time a file can be accessed via a temporary url.
maxTempUrlAlive :: NominalDiffTime
maxTempUrlAlive = 60 * 60 * 24

topNavigator :: SectionId -> Widget
topNavigator current = [whamlet|
<section>
    ^{renderBrowsable current}
    ^{renderSearch current}
    |]

getMediaHomeR :: Handler Html
getMediaHomeR = do
    ent <- requireAuth
    defaultLayout $ do
        setTitle "Media"
        navigation "Media"
        wrapMain $ do
            topNavigator ""
            userPlaylistWidget ent

getMediaContentR :: Text -> FPS -> Handler Html
getMediaContentR section fps = do
    ent <- requireAuth
    renderMaybeBare content $ do
        setTitle "Media"
        wrapMain $ do
            topNavigator section
            layoutSplitH (browser content [".input-search + button"])
                         (userPlaylistWidget ent)
  where content = sectionWidget section fps

getMediaSearchAllR :: Handler Html
getMediaSearchAllR = do
    ent <- requireAuth
    mq  <- lookupGetParam "q"
    let doSearch q
          | T.null q  = do
            let msg = "Search must be at least one character!" :: Html
            renderMaybeBare [whamlet|<i>#{msg}|] $ liftHandlerT $ do
                setMessage msg
                redirect MediaHomeR

          | otherwise = do
            rs' <- onSections (\s -> (\source -> browsableRender source ["Search results for \n" <> T.unpack q <>"\n"] s) $ searchableSearchT q s) -- :: forall source. MediaSearchable App source => source -> Widget)
            rs  <- liftM mconcat $ sequence $ Map.elems $ Map.mapWithKey f rs'
            --results  <- liftM mconcat $ sequence $ Map.elems $ Map.mapWithKey f results'
            renderMaybeBare rs $ do
                setTitle $ toHtml $ "Results for: " <> q
                wrapMain $ do
                    topNavigator ""
                    layoutSplitH (browser rs [".input-search + button"])
                                 (userPlaylistWidget ent)
        in maybe (redirect MediaHomeR) doSearch mq
  where
      f section hres = hres >>= \res -> return [whamlet|
<h3>#{section}
^{res}
|] 

-- | search in section @section@ using query string from the @q@ get parameter.
getMediaSearchR :: SectionId -> Handler Html
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
            results <- onSection section (\s -> (\source -> browsableRender source ["Search results for \n" <> T.unpack q <> "\n"] s) $ searchableSearchT q s) -- :: forall source. MediaSearchable App source => source -> Widget)
            renderMaybeBare results $ do
                setTitle $ toHtml $ "Results for: " <> q
                wrapMain $ do
                    topNavigator section
                    layoutSplitH (browser results [".input-search + button"])
                                 (userPlaylistWidget ent)
    maybe (redirect $ MediaContentR section []) doSearch mq

-- * Widgets

renderMaybeBare :: Widget -> Widget -> Handler Html
renderMaybeBare bareContents nonBareContents = do
    mq <- lookupGetParam "bare"
    case mq of
        Just _  -> widgetBodyToRepHtml bareContents
        Nothing -> defaultLayout $ do
            navigation "Media"
            nonBareContents

-- | Generate content based on section and path.
sectionWidget :: Text -> FPS -> Widget
sectionWidget s fps = join $ liftHandlerT $ onSection s (browsableFetchWidget fps)

renderSearch :: SectionId -> Widget
renderSearch      "" = renderSearchAll
renderSearch section = [whamlet|$newline never
<form .bare .text-center action=@{MediaSearchR section} type=get>
  <input .input-search type="search" name="q" placeholder="Search #{section}" autofocus pattern="..*" required>
  <button .btn type="submit">Go
|]

renderSearchAll :: Widget
renderSearchAll = [whamlet|$newline never
<form .bare .text-center action=@{MediaSearchAllR} type=get>
  <input .input-search type="search" name="q" placeholder="Search all" autofocus pattern="..*" required>
  <button .btn type="submit">Go
|]

-- * Playing & Downloading

-- | Downloading files.
-- /tmp does not require authentication, other kinds do.
getMediaServeR :: ServeType -> SectionId -> FPS -> Handler RepJson
getMediaServeR     _       _   [] = invalidArgs ["No target provided."]
getMediaServeR stype section path = case stype of
    ServeTemp           -> solveTemp         >>= send ""
    ServeAuto           -> solvePathWithAuth >>= send ""
    ServeForceDownload  -> solvePathWithAuth >>= send "application/force-download"
  where
    send ct fp = addHeader "Accept-Ranges" "bytes" >> sendFile ct fp

    solveTemp  = do
        Entity _ (DlTemp time _ realpath) <- runDB $ getBy404 $ UniqueDlTemp $ head path
        now <- timeNow
        denyIf (diffUTCTime now time > maxTempUrlAlive) "File not available."
        denyIf (FP.joinPath (tail path) /= realpath      ) "Malformed url."
        join $ onSection section (browsableFetchPlain path) -- :: forall s. MyMedia Handler s => s -> Handler FilePath)

    solvePathWithAuth = do
        uid <- requireAuthId
        fp  <- join $ onSection section (browsableFetchPlain path)
        t   <- timeNow
        _   <- runDB $ insert $ LogDownload t uid section path -- TODO: use a log file?
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
postMediaAdminR :: Handler Html
postMediaAdminR = do
    ((result, _), _) <- runFormPost adminForm
    case result of
        FormSuccess (True,_) -> mediaUpdateR
        FormSuccess _        -> setMessage "No actions."
        _                    -> setMessage "Form failed!"
    redirect AdminR

mediaUpdateR :: Handler ()
mediaUpdateR = do
    ras <- updateAllMedia
    runDB $ mapM_ insert ras
    setMessage $ toHtml $ "Added " <> (show . length) ras <> " new items."

mediaRecent :: Int -> Widget
mediaRecent n = do
    recent <- liftHandlerT $ runDB $
        selectList [] [Desc RecentlyAddedDate, LimitTo n]
    [whamlet|
<ul>
    $forall Entity _ ra <- recent
      <li>
        <a href=@{MediaContentR (recentlyAddedSection ra) (recentlyAddedParts ra)}>
            #{recentlyAddedDesc ra}
        <small>
            <i>Added #{printfTime "%d.%m" $ recentlyAddedDate ra}
|]

mediaRecentDl :: Int -> Widget
mediaRecentDl n = do
    recent <- liftHandlerT $ runDB $ selectList [] [Desc LogDownloadTime, LimitTo n]
    [whamlet|
<ul>
    $forall Entity _ dl <- recent
        <li>
            <a href=@{MediaContentR (logDownloadSection dl) (logDownloadFps dl)}>
                #{last $ logDownloadFps dl}
            <small>
                <i>Played #{printfTime "%d.%m %H:%M" $ logDownloadTime dl}
|]

adminForm :: Form (Bool, Bool)
adminForm = renderBootstrap $ (,)
     <$> areq boolField "Update every index" (Just False)
     <*> areq boolField "Not used" (Just False)
