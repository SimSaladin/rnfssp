{-# LANGUAGE NoImplicitPrelude, RankNTypes, TemplateHaskell, QuasiQuotes, OverloadedStrings, ConstraintKinds, FlexibleInstances, FlexibleContexts, TypeFamilies, GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module MediaSub.Playlist
    ( getPlaylistR
    , postPlaylistR
    , userPlaylistWidget
    , defaultLayoutMedia
    ) where

import qualified Data.Conduit.List as CL
import           Data.Conduit
import           Control.Monad (zipWithM)
import qualified System.FilePath as FS
import           System.Directory (getTemporaryDirectory)
import           System.IO.Temp (openTempFile)

import           MediaSub.Import

defaultLayoutMedia :: Yesod m => Widget -> SubHandler m Html
defaultLayoutMedia widget = defaultLayoutSub $ do
    widget
    userPlaylistWidget

-- | GET actions on playlists. 'action' is based on
--  * get.*       : Get the active playlist with @solvePlaylist@ and send it as
--    m3u.
--  * get.*-force : include force-download -header.
-- 
getPlaylistR :: Text -> Handler RepPlain
getPlaylistR action = do
    x <- handlerActionIdent >>= solvePlaylist >>= generateM3U . entityVal
    case action of
        "play"     -> sendFile "audio/x-mpegurl" x
        "download" -> do
            addHeader "Content-Disposition" "attachment; filename=\"playlist.m3u\""
            sendFile "application/force-download" x
        _ -> invalidArgs ["Invalid or unsupported action: " <> action]

-- | JSON interface and perform actions on playlists. action is the first part
-- in the uri.
--
-- action is one of:
--    select: Get active playlist and send it as JSON.
--    insert: Adds an filenode and its children to current playlist, and
--            save the playlist.
--    clear:  remove all elements from the playlist. TODO
--    delete: Delete selected elements. TODO
--
postPlaylistR :: Text -> Handler Value
postPlaylistR action = do
    Entity plk pl <- solvePlaylist =<< handlerActionIdent
    case action of
      "select"  -> rsucc pl
      "insert"  -> parseJsonBody_ >>= uncurry (plInsert pl)
                                  >>= maybe (rfail "No such path!") (plUpdate plk >=> rsucc)
      "clear"   -> plUpdate plk (plClear pl) >>= rsucc
      "delete"  -> parseJsonBody_ >>= plUpdate plk . plDelete pl >>= rsucc
      _ -> rfail "Unknown playlist action."
  where
    rfail :: Text -> Handler Value
    rfail msg = returnJson (1 :: Int, msg)
    rsucc msg = returnJson (0 :: Int, msg)

-- | Playlist widget customized for a user.
userPlaylistWidget :: Widget
userPlaylistWidget = do
    [mainI, actionsI, contI, headI] <- replicateM 4 newIdent
    $(widgetFile "media-playlist")

getPlaylistListR :: Handler Html
getPlaylistListR = undefined

-- * Actions

-- | New titleless playlist for user with userId uid.
-- XXX: doesn't check for duplicates(!)
addDefaultPlaylist :: Handler (Entity Playlist)
addDefaultPlaylist = do
    uid <- handlerActionIdent
    pl <- liftM (\t -> Playlist "" uid [] t t) timeNow
    pid <- lift $ runDB $ insert pl
    return (Entity pid pl)

-- | Get all playlists (whose == Nothing) or a user's (whose == Just UserId) playlists.
plGet :: Handler [Entity Playlist]
plGet = do
    uid <- handlerActionIdent
    lift $ runDB $ selectList [PlaylistOwner ==. uid] []

plClear :: Playlist -> Playlist
plClear pl = pl { playlistElems = [] }

plDelete :: Playlist -> Int -> Playlist
plDelete pl@Playlist{playlistElems = xs} i =
    pl{ playlistElems = removeByIndex i xs }

-- | saves (replaces) an existing playlist. This is unsafe (replace) due to
-- unique constraints. TODO: check title for uniqueness.
plUpdate :: PlaylistId -- ^ id of playlist to replace
         -> Playlist
         -> Handler Playlist -- ^ was update successful?
plUpdate plid pl = lift (runDB $ replace plid pl) >> return pl

-- | add file OR every child of directory to playlist.
plInsert :: Playlist -> SectionId -> FPS -> Handler (Maybe Playlist) -- maybe changed playlist
plInsert pl sid fps = lift $ do
    section <- mediaGetSection sid
    items <- secGetPlayables section fps $$ CL.consume
    case items of
        [] -> return Nothing
        _  -> do
            time <- timeNow
            return $ Just $ pl
                 { playlistElems    = playlistElems pl ++ map ((,) sid) items
                 , playlistModified = time
                 }

-- * Helpers

-- | Retrieve user's playlist using various methods. If all else fails, create
-- a default playlist. order: getparam -> cookie -> database -> new playlist
--
-- if "title" get parameter is set and no playlist is found, result is 404.
--
-- NOTE: Playlist with a title of "" is the default for any user.
--
-- XXX: support hidden playlists?
solvePlaylist :: Text -> Handler (Entity Playlist)
solvePlaylist uid = fromGetParam
  where
    fromGetParam = lift (lookupGetParam "playlist") >>= maybe fromCookie (getOr fromCookie)
    fromCookie   = lift (lookupCookie   "playlist") >>= maybe fromDefault (getOr fromDefault)
    fromDefault  = lift (runDB (getBy $ UniquePlaylist uid "")) >>= tryMaybe addDefaultPlaylist

    getOr tryThis title = lift (runDB (getBy $ UniquePlaylist uid title)) >>= tryMaybe tryThis

-- | Generate a m3u-formatted file of the playlist with random-generated
-- check-texts for access from any(?) client.
-- Saves to a temporary file.
-- File format:
--
-- > #EXTM3U
-- > #EXTINF:length, extra_info
-- > @{MediaServeR ServeTemp hash path}
-- > ...
--
generateM3U :: Playlist -> Handler String
generateM3U pl = do
    (playlistFilePath, h) <- liftIO $ getTemporaryDirectory >>= flip openTempFile "playlist.m3u"
    MediaSub{..} <- getYesod
    renderer    <- lift getUrlRender
    time        <- timeNow
    toParent    <- getRouteToParent

    let serveRoute sec fps temp = renderer . toParent $ MediaServeR ServeTemp sec (temp : fps)

        logEntry temp (sec, path) = insert $ DlTemp time temp sec path

        -- write single file entry to handle. calculate a temporary name.
        write (sec, path) = do
            temp <- randomText 32 :: IO Text
            hPutStrLn h $ serveRoute sec (map pack . FS.splitPath $ unpack path) temp
            return temp

    liftIO $ hPutStrLn h ("#EXTM3U\n" :: Text)
    temps <- liftIO $ mapM write paths
    liftIO $ hClose h
    _ <- lift $ runDB $ zipWithM logEntry temps paths
    return playlistFilePath
    where
        paths = playlistElems pl

handlerActionIdent :: Handler Text
handlerActionIdent =
    -- XXX: one could act on someone else's playlist
    lift mediaIdent
