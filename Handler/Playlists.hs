{-# LANGUAGE DoAndIfThenElse #-}
------------------------------------------------------------------------------
-- File:          Handler/Playlists.hs
-- Creation Date: Dec 23 2012 [22:08:10]
-- Last Modified: Dec 24 2012 [15:35:33]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Handler.Playlists
    ( getPlaylistR
    , postPlaylistR
    , userPlaylistWidget
    ) where

import           Utils
import           Import
import           Configs
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           System.IO (hClose)
import           System.Directory (getTemporaryDirectory)
import           System.IO.Temp (openTempFile)

-- | GET actions on playlists. 'action' is based on
--  * get.*       : Get the active playlist with @solvePlaylist@ and send it as
--    m3u.
--
--  * get.*-force : include force-download -header.
-- 
getPlaylistR :: Text -> Handler RepPlain
getPlaylistR action
    | action `T.isPrefixOf` "get" = requireAuth
      >>= solvePlaylist
      >>= generateM3U . entityVal
      >>= if "-force" `T.isSuffixOf` action
        then (setHeader "Content-Disposition" "attachment; filename=\"playlist.m3u\"" >>) 
             . sendFile "application/force-download"
        else sendFile "audio/x-mpegurl"
    | otherwise = invalidArgs ["Invalid or unsupported action: " `T.append` action]

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
postPlaylistR :: Text -> Handler RepJson
postPlaylistR action = do
    Entity plk pl <- solvePlaylist =<< requireAuth
    case action of
      "select" -> rsucc pl
      "insert" -> do
          (section, what) <- parseJsonBody_
          (new, pl') <- add pl section what
          if new
            then updatePlaylist plk pl' >>= rsucc
            else rfail "no such path"
      "delete"  -> updatePlaylist plk (clearPlaylist pl) >>= rsucc
      _ -> rfail "unknown playlist action"
  where
    rfail :: Text -> Handler RepJson
    rfail msg = jsonToRepJson (1 :: Int, msg)
    rsucc msg = jsonToRepJson (0 :: Int, msg)

-- | Playlist widget customized for a user.
userPlaylistWidget :: Entity User -> Widget
userPlaylistWidget (Entity _ uval) = do
    [main, actions, content, heading] <- replicateM 4 (lift newIdent)
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
solvePlaylist (Entity k v) = fromGetparam
  where
    fromGetparam = lookupGetParam "title" >>= \mt -> case mt of
        Just title -> runDB (getBy (UniquePlaylist k title)) >>= tryMaybe fromCookie 
        Nothing    -> fromCookie

    fromCookie = lookupCookie "playlist-title" >>= \mt' -> case mt' of
        Just title -> runDB (getBy (UniquePlaylist k title)) >>= tryMaybe fromDB
        Nothing    -> fromDB

    fromDB = case userCurrentplaylist v of
        Just name -> runDB (getBy $ UniquePlaylist k name) >>= tryMaybe fromDBDefault
        Nothing   -> fromDBDefault

    fromDBDefault = runDB (getBy $ UniquePlaylist k "") >>= tryMaybe (addDefaultPlaylist k)

-- | Generate a m3u-formatted file of the playlist with random-generated
-- check-texts for access from any(?) client.
-- Saves to a temporary file.
-- File format:
--
-- > #EXTM3U
-- > #EXTINF:length, extra_info
-- > @{MediaServeR "temp" hash path}
-- > ...
--
generateM3U :: Playlist -> Handler FilePath
generateM3U pl = do
    resolved <- gServeroot
    yesod    <- getYesod
    time     <- timeNow
    let render a p t = yesodRender
          yesod resolved (MediaServeR "temp" a (t : splitPath' p)) []

        write h (area, path) = do
            temp <- randomText 32
            T.hPutStrLn h (render area path temp)
            return (temp, path)

    (path, temps) <- liftIO $ do
      (path, handle) <- getTemporaryDirectory >>= flip openTempFile "playlist.m3u"
      T.hPutStrLn handle "#EXTM3U\n"
      temps <- mapM (write handle) $ playlistElems pl
      hClose handle
      return (path, temps)
    mapM_ (uncurry $ ((runDB . insert) .) . DlTemp time) temps
    return path


-- * Actions

clearPlaylist :: Playlist -> Playlist
clearPlaylist pl = pl { playlistElems = [] }

-- | New titleless playlist for user with userId uid.
-- XXX: doesn't check for duplicates(!)
addDefaultPlaylist :: UserId -> Handler (Entity Playlist)
addDefaultPlaylist uid = do
    t <- timeNow
    let pl = Playlist "" uid [] t t
    k <- runDB $ insert pl
    return $ Entity k pl

-- | saves (replaces) an existing playlist. This is unsafe (replace) due to
-- unique constraints. TODO: check title for uniqueness.
updatePlaylist :: PlaylistId -- ^ id of playlist to replace
               -> Playlist
               -> Handler Playlist -- ^ was update successful?
updatePlaylist plid pl = runDB (replace plid pl) >> return pl

-- | Get all playlists (whose == Nothing) or a user's (whose == Just UserId) playlists.
--
-- TODO NOT YET IMPLEMENTED
get :: Maybe UserId -> Handler [Entity Playlist]
get whose = runDB $ selectList ([] :: [Filter Playlist]) []

-- | add file OR every child of directory to playlist.
add :: Playlist
    -> Text -- ^ File area
    -> Text -- ^ File path
    -> Handler (Bool, Playlist) -- (playlist changed?, changed playlist)
add pl section path = do
    t <- timeNow
    these' <- onSec section $ flip findr path
    return $ case these' of
      [] -> (False, pl)
      these -> (True, pl{ playlistElems = playlistElems pl ++ map ((,) section) these
                     , playlistModified = t 
                     })
