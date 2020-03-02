{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE RecordWildCards        #-}

-- | MediaSub handlers.
module MediaSub.Media where

import           MediaSub.Import
import           MediaSub.Browser (browser, BrowserSettings(..), pagerSettings)
import           MediaSub.Playlist (userPlaylistWidget)
import           Data.Time.Clock (diffUTCTime)
import qualified Data.Map as Map

myBrowser :: WidgetT master IO () -> WidgetT master IO ()
myBrowser = browser . BrowserSettings
        "mybrowser" "a.browser-link" [".input-search + button"]

lookupSection :: YesodMediaSub m => SectionId -> SubHandler m (Section m)
lookupSection sid = maybe (invalidArgs ["No such section"]) return $ Map.lookup sid mediaSections

-- * Handlers

-- | The home view consist of different widgets to provide an overview of
-- media status, especially every section's custom widget.
getMediaHomeR :: forall m. YesodMediaSub m => SubHandler m Html
getMediaHomeR =
    defaultLayoutSub $ do
        setTitle "Media"
        $(widgetFile "media-home")
  where
    secs = Map.assocs mediaSections :: [(SectionId,  Section m)]

-- | Serve files.
-- /tmp does not require authentication, others should.
getMediaServeR :: ServeType -> SectionId -> FPS -> Handler RepJson
getMediaServeR     _       _  [] = invalidArgs ["No target provided."]
getMediaServeR stype section fps = case stype of
    ServeTemp           -> solveTemp         >>= send "" . fpToString
    ServeAuto           -> solvePathWithAuth >>= send "" . fpToString
    ServeForceDownload  -> solvePathWithAuth >>= send "application/force-download" . fpToString
  where
    send ct fp = addHeader "Accept-Ranges" "bytes" >> sendFile ct fp

    solveTemp  = do
        Entity _ (DlTemp time _ _ path) <-
            lift . runDB . getBy404 . UniqueDlTemp $ unsafeHead fps
        now <- timeNow
        MediaSub{..} <- getYesod
        lift $ denyIf (diffUTCTime now time > mediaMaxTempUrlAlive) "File not available."
        lift $ denyIf (fpsToFilePath (unsafeTail fps) /= fpFromText path) "Malformed url."
        lift (mediaGetSection section) >>= (`secRealFile` fps)

    solvePathWithAuth = do
        ident <- lift mediaIdent
        path  <- lift (mediaGetSection section) >>= (`secRealFile` fps)
        now   <- timeNow
        lift . void . runDB . insert $ LogDownload now ident section (concat fps)
        return path

getMediaContentR :: SectionId -> FPS -> Handler TypedContent
getMediaContentR sid fps = do
    section <- lookupSection sid
    secGetContents section fps

   --  paging <- liftHandlerT pagerSettings
--     defaultLayoutMedia $ do
--         setTitle "Media"
--         myBrowser 

-- * Widgets

searchWidget :: Maybe SectionId -> Widget
searchWidget sec = $(widgetFile "search-form")
    where route = maybe MediaSearchAllR MediaSearchR sec

-- * Unreviewed

-- <div .browser>
--   <div .text-center .hl-on-hover>^{browseNavigation}
--   $forall (fps, (ftype, xs)) <- elements
--     <div .entry .type-#{ftype}>
--         <div .data-field style="display:none">#{F.joinPath fps}
--         <div .browser-controls>
--           $if (/=) ftype directory
--             <a .icon-download href=@{(lfServeR config) ServeForceDownload fps} onclick="">
--             <a .icon-play     href=@{(lfServeR config) ServeAuto fps}          onclick="" target="_blank">
--           <button .icon-plus .action onclick="playlist.to_playlist('#{lfSec config}', [$(this).closest('.entry').children()[0].innerText]); return false">
--         <a .browser-link .#{ftype} href=@?{ ( (lfViewR config) fps, parameters )}>
--             <span .filename>#{last fps}
--             <span .misc>
--               $forall (desc, value) <- xs
--                 <span><i>#{desc}:</i> #{value}
--   <div .text-center .hl-on-hover style="margin-top:0.5em;">^{browseNavigation}
--     |]
--   where
--     directory   = "directory"
--     parameters  = [] -- TODO:    if' (perpage == 0) [] [("limit_to", T.pack $ show perpage)]
--     playlistAddAll = [whamlet|
-- <a .btn
--     onclick="playlist.add_from_element_contents($('.browser .type-file .data-field'), '#{lfSec config}'); return false"
--     title="Adds all files in this folder.">Add all
--     |]
--     (browseSettings, browseNavigation) = (pagerRender <$> lfPaging
--                                                       <*> lfNumPages
--                                                       <*> (lfViewR <$> id <*> lfCurrentFPS) ) config
--     brwidget = (simpleBreadcrumbs <$> lfSec
--                                   <*> lfCurrentFPS
--                                   <*> lfViewR) config
-- 

mediaSingleControls :: SectionId -> FPS
                    -> (ServeType -> SectionId -> FPS -> Route master) -- ^ ServeR
                    -> WidgetT master IO ()
mediaSingleControls secid fps serveR =
        [whamlet|<i>mediaSingleControls: uups wut?|]
--         [whamlet|
-- <div .text-center>
--     <div .btn-group>
--       <a .btn .btn-primary href="@?{hauto}" target="_blank"><i .icon-white .icon-play></i>
--           Auto-open
--       <a .btn href="@{hforce}"><i .icon .icon-download-alt></i>
--           Download
--       <a .btn onclick="window.playlist.to_playlist('#{secid}', ['#{fpsToFilePath fps}']); return false">
--           To playlist
-- |] where hauto  = (serveR ServeAuto          secid fps, [])
--          hforce =  serveR ServeForceDownload secid fps

getMediaSearchAllR :: Handler TypedContent
getMediaSearchAllR =
    getSearchHelperR $ \q -> do
        sections <- lift mediaGetSections
        let sources = map (\(_, s) -> secSearchWild s q) sections
        undefined

-- | search in section @section@ using query string from the @q@ get parameter.
getMediaSearchR :: SectionId -> Handler TypedContent
getMediaSearchR section =
    getSearchHelperR $ \q -> do
        source <- liftM (`secSearchWild` q) $ lift $ mediaGetSection section
        undefined

getSearchHelperR :: (Text -> SubHandler m TypedContent) -> SubHandler m TypedContent
getSearchHelperR doSearch = do
    mq <- lookupGetParam "q"
    case mq of
        Nothing -> goHomeEmpty
        Just "" -> goHomeEmpty
        Just q  -> doSearch q
  where
      emptyMsg = "Search must be at least one character!" :: Html
      goHomeEmpty = setMessage emptyMsg >> redirect MediaHomeR

getMediaAdminR :: SubHandler m Html
getMediaAdminR = do
    undefined

-- | Documentation for 'postMediaAdminR'
postMediaAdminR :: Handler Html
postMediaAdminR = error "undefined: `postMediaAdminR' in MediaSub/Media.hs"

-- * Widgets

-- | N most recently added things
mediaRecent :: Int -> Widget
mediaRecent n = [whamlet|<i>ups, broken! |]
--     recent <- liftHandlerT . runDB . liftM (map entityVal) $
--         selectList [] [Desc RecentlyAddedDate, LimitTo n]
--     rdates <- liftIO $ mapM (formatTimeZoned "%d.%m" . recentlyAddedDate) recent
--     [whamlet|
-- <ul>
--     $forall (ra, date) <- zip recent rdates
--       <li>
--         <a href=@{MediaContentR (recentlyAddedSection ra) (recentlyAddedParts ra)}>
--             #{recentlyAddedDesc ra}
--         <small>
--             <i>Added #{date}
-- |]

mediaRecentDl :: Int -> Widget
mediaRecentDl n = 
    [whamlet|<i>Uups, broken|]
--     recent <- liftHandlerT . runDB . liftM (map entityVal) $
--         selectList [] [Desc LogDownloadTime, LimitTo n]
--     rdates <- liftIO $ mapM (formatTimeZoned "%d.%m %H:%M" . logDownloadTime) recent
--     [whamlet|
-- <ul>
--     $forall (dl, date) <- zip recent rdates
--         <li>
--             <a href=@{MediaContentR (logDownloadSection dl) (logDownloadFps dl)}>
--                 #{last $ logDownloadFps dl}
--             <small><i>Played #{date}</i>
-- |]
