{-# LANGUAGE DoAndIfThenElse #-}
------------------------------------------------------------------------------
-- File:          Handler/Playlists.hs
-- Creation Date: Dec 23 2012 [22:08:10]
-- Last Modified: Oct 11 2013 [11:09:30]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Handler.Playlists
    ( getPlaylistR
    , postPlaylistR
    , userPlaylistWidget
    ) where

import           Utils
import           Import
import qualified Data.Conduit.List as CL
import           Data.Conduit
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.FilePath as FS
import           System.IO (hClose, hPutStrLn)
import           System.Directory (getTemporaryDirectory)
import           System.IO.Temp (openTempFile)

import           Sections

-- | GET actions on playlists. 'action' is based on
--  * get.*       : Get the active playlist with @solvePlaylist@ and send it as
--    m3u.
--  * get.*-force : include force-download -header.
-- 
getPlaylistR :: Text -> Handler RepPlain
getPlaylistR action = do
    x <- requireAuth >>= solvePlaylist >>= generateM3U . entityVal
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
    Entity plk pl <- solvePlaylist =<< requireAuth
    case action of
      "select"  -> rsucc pl
      "insert"  -> parseJsonBody_ >>= uncurry (plInsert pl) >>= \(new, pl') ->
          if new
            then plUpdate plk pl' >>= rsucc
            else rfail "No such path."
      "clear"   -> plUpdate plk (plClear pl) >>= rsucc
      "delete"  -> parseJsonBody_ >>= plUpdate plk . plDelete pl >>= rsucc
      _ -> rfail "Unknown playlist action."
  where
    rfail :: Text -> Handler Value
    rfail msg = returnJson (1 :: Int, msg)
    rsucc msg = returnJson (0 :: Int, msg)

getPlaylistListR :: Handler Html
getPlaylistListR = undefined

-- | Playlist widget customized for a user.
userPlaylistWidget :: Entity User -> Widget
userPlaylistWidget (Entity _ uval) = do
    [mainI, actionsI, contI, headI] <- replicateM 4 (liftHandlerT newIdent)
    $(widgetFile "media-playlist")

-- | Retrieve user's playlist using various methods. If all else fails, create
-- a default playlist. order: getparam -> cookie -> database -> new playlist
--
-- if "title" get parameter is set and no playlist is found, result is 404.
--
-- NOTE: Playlist with a title of "" is the default for any user.
--
-- XXX: support hidden playlists?
solvePlaylist :: Entity User -> Handler (Entity Playlist)
solvePlaylist (Entity k v) = fromGetparam where
    fromGetparam = lookupGetParam "title" >>= \mt -> case mt of
        Just title  -> runDB (getBy $ UniquePlaylist k title) >>= tryMaybe fromCookie 
        Nothing     -> fromCookie

    fromCookie = lookupCookie "playlist-title" >>= \mt' -> case mt' of
        Just title  -> runDB (getBy $ UniquePlaylist k title) >>= tryMaybe fromDB
        Nothing     -> fromDB

    fromDB = case userCurrentplaylist v of
        Just name   -> runDB (getBy $ UniquePlaylist k name) >>= tryMaybe fromDBDefault
        Nothing     -> fromDBDefault

    fromDBDefault = runDB (getBy $ UniquePlaylist k "") >>= tryMaybe (addDefaultPlaylist k)

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
generateM3U :: Playlist -> Handler FilePath
generateM3U pl = do
    resolved <- gServeroot
    yesod    <- getYesod
    let render a p t = yesodRender yesod resolved (MediaServeR ServeTemp a (t : FS.splitPath p)) []
        -- write single file entry to handle. calculate a temporary name.
        write h (area, path) = do
            temp <- randomText 32
            T.hPutStrLn h (render area path $ T.unpack temp)
            return (temp, path)

    (path, temps) <- liftIO $ do
        (path, handle) <- getTemporaryDirectory >>= flip openTempFile "playlist.m3u"
        hPutStrLn handle "#EXTM3U\n"
        temps <- mapM (write handle) $ playlistElems pl
        hClose handle
        return (path, temps)

    time <- timeNow
    mapM_ (\(ident, realpath) -> runDB $ insert $ DlTemp time (T.unpack ident) realpath) temps
    return path

-- * Actions

-- | New titleless playlist for user with userId uid.
-- XXX: doesn't check for duplicates(!)
addDefaultPlaylist :: UserId -> Handler (Entity Playlist)
addDefaultPlaylist uid = do
    pl <- liftM (\t -> Playlist "" uid [] t t) timeNow
    liftM (flip Entity pl) . runDB $ insert pl

-- | Get all playlists (whose == Nothing) or a user's (whose == Just UserId) playlists.
plGet :: Maybe UserId -> Handler [Entity Playlist]
plGet (Just uid) = runDB $ selectList [PlaylistOwner ==. uid] []
plGet Nothing    = runDB $ selectList                      [] []

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
plUpdate plid pl = runDB (replace plid pl) >> return pl

-- | add file OR every child of directory to playlist.
plInsert :: Playlist -> SectionId -> FPS -> Handler (Bool, Playlist) -- (playlist changed?, changed playlist)
plInsert pl section paths = do
    t <- timeNow
    elemSource <- onSection section (browsableFetchPlainR paths)
    these'     <- elemSource $$ CL.consume
    return $ case these' of
        []    -> (False, pl)
        these -> (True, pl{ playlistElems = playlistElems pl ++ map ((,) section) these
                     , playlistModified = t 
                     })
