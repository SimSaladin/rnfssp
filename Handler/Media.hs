{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, DoAndIfThenElse #-}
module Handler.Media
    ( getMediaR
    , postMediaR
    , getMediaServeR

    , getMediaAdminR
    , postMediaAdminR

    , getMediaPlaylistR
    , postMediaPlaylistR
    ) where

import           Utils
import           Import
import           Data.List ((\\), init, last, head, tail)
import           Data.Char (chr)
import           Data.Maybe (isNothing)
import           Data.Time.Clock (diffUTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format (formatTime)
import           Control.Arrow (first)
import           Control.Monad
import qualified Control.Monad.Random as MR (evalRandIO, getRandomR)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.FilePath as F (joinPath)
import           System.Directory (getDirectoryContents, getTemporaryDirectory)
import           System.FilePath (takeDirectory, takeFileName, (</>), normalise, splitPath)
import           System.IO (hClose)
import           System.IO.Temp (openTempFile)
import           System.Locale (defaultTimeLocale)
import           System.Posix.Files (FileStatus, getFileStatus, fileSize, modificationTime, isDirectory)
import           System.Process (readProcessWithExitCode)

-- * Confs & Utilities

viewable :: [Text]
viewable = ["audio", "video"]

isdir :: Text -> Bool
isdir = (==) "directory"

toPath :: [Text] -> Text
toPath = T.pack . F.joinPath . map T.unpack


-- * Userspace / General

-- | The root and heart of media section. Loads browser, playlist and other
--   possibly widgets. 
getMediaR :: [Text] -> Handler RepHtml
getMediaR fps = do
    mauth <- maybeAuth
    bare <- lookupGetParam "bare"
    case bare of
        Just _ -> browserTable fps
        Nothing -> defaultLayout $ do
            setTitle "Media"
            $(widgetFile "media-home")
          where mediaBrowser = browserWidget fps

-- | Used to modify media -- TODO TODO TODO --
postMediaR :: [Text] -> Handler RepHtml
postMediaR fps = do
    setMessage "This is not yet supported"
    redirect $ MediaR fps

getMediaServeR :: Text   -- ^ kind of download
               -> Text   -- ^ file area
               -> [Text] -- ^ path for the file
               -> Handler RepJson
getMediaServeR kind area path
  | null path  = invalidArgs ["Invalid download target"]
  | is "auto"  = authPath >>= send ""
  | is "force" = authPath >>= send "application/force-download"
  | is "temp"  = do
      Entity _ (DlTemp time _ target) <- runDB $ getBy404 $ UniqueDlTemp ident
      now <- timeNow
      denyIf (diffUTCTime now time > maxTime) "This temporary url has expired!"
      denyIf (target /= toPath (tail path)) "Malformed url"
      send "" . toReal target =<< gdir area
  | otherwise  = invalidArgs ["Invalid/unsupported download type"]
  where
    is         = (==) kind
    ident      = head path
    send t p   = setHeader "Accept-Ranges" "bytes" >> sendFile t p
    maxTime    = 60 * 60 * 24
    toReal t r = r </> T.unpack t
    authPath   = do
        uid <- requireAuthId
        Entity key val <- runDB $ getBy404 $ UniqueFilenode area (toPath path)
        if filenodeIsdir val
          then invalidArgs ["Downloading directories is not supported"]
          else do t <- timeNow
                  _ <- runDB $ insert $ LogDownload uid t key
                  return . toReal (filenodePath val) =<< gdir area


-- * Browser

-- | the <table> element only
browserTable :: [Text] -> Handler RepHtml
browserTable fps = do
    _ <- requireAuth
    pc <- widgetToPageContent $ browserViewWidget fps
    hamletToRepHtml [hamlet|^{pageBody pc}|]

-- | The driver for the browser
browserWidget :: [Text] -> Widget
browserWidget fps = do
    browserId <- lift newIdent
    $(widgetFile "browser-driver")
  where
    sections = map (\(sect, icon) -> (current == sect, MediaR [sect], sect, icon)) browsable
    current  = if' (null fps) "" (head fps)

browsable :: [(Text, Text)]
browsable = [ ("anime", "film"), ("music", "music")]

-- | requires the browser script already on the page
browserViewWidget :: [Text] -> Widget
browserViewWidget [] = let
    nav     = [] :: [(Text,Texts)]
    area    = "" :: Text
    is_dir  = False
    is_file = False
    details = Nothing :: Maybe String
    listing = [] :: [(String, Text, [Text], [Text], Text, Text)]
    in $(widgetFile "browser-bare")
browserViewWidget (area:path) = do
    node <- lift $ runDB $ getBy404 $ UniqueFilenode area (T.pack $ normalise $ F.joinPath $ map T.unpack path)
    let val     = entityVal node
        is_dir  = filenodeIsdir val
        is_file = not is_dir
        details = filenodeDetails val

    nodes <- if is_file
      then return []
      else lift $ runDB $ selectList [ FilenodeParent ==. (Just $ entityKey node)
                                     , FilenodePath   !=. "." ] [Asc FilenodePath]

    let listing = do
        this <- map entityVal nodes
        let file     = takeFileName $ T.unpack $ filenodePath this
            filetype = if filenodeIsdir this
                         then "directory"
                         else guessFiletype file
            fps      = path ++ [T.pack file]
            size     = filenodeSize this
            modified = formatTime defaultTimeLocale "%d.%m -%y" $ filenodeModTime this
--            details  = filenodeDetails this
            in return (file, filetype, fps, area : fps, size, modified)
    $(widgetFile "browser-bare")
  where
    nav = zip (area:path) (foldr (\x xs -> [x] : map ([x] ++) xs) [[]] (area:path))


-- * Playing & Playlists

-- | Create and send a new playlist file (.m3u). playlist is got via
-- @getPlaylist@.
getMediaPlaylistR :: Text -> Handler RepHtml
getMediaPlaylistR action
    | is "get"  = requireAuth
      >>= getPlaylist
      >>= generateM3U . entityVal
      >>= if force
        then \x -> do setHeader "Content-Disposition"
                                "attachment; filename=\"playlist.m3u\""
                      sendFile "application/force-download" x
        else sendFile "audio/x-mpegurl"
    | otherwise = invalidArgs ["action not supported: " `T.append` action]
  where
    is = flip T.isPrefixOf action
    force = "-force" `T.isSuffixOf` action

-- | action: "select" returns current or creates a new playlist
--           "push" adds an filenode and its children to current playlist and
--                  saves it if possible.
postMediaPlaylistR :: Text -> Handler RepJson
postMediaPlaylistR action = do
    uent <- requireAuth
    Entity plk pl <- getPlaylist uent
    case action of
      "select" -> rsucc pl
      "push"   -> do
          (area, what) <- parseJsonBody_
          (new, pl') <- toPlaylist pl area what
          if new
            then updatePlaylist plk pl' >>= rsucc
            else rfail "no such path"
      "clear"  -> updatePlaylist plk (clearPlaylist pl) >>= rsucc
      _ -> rfail "unknown playlist action"
  where
    rfail :: Text -> Handler RepJson
    rfail msg = jsonToRepJson (1 :: Int, msg)
    rsucc msg = jsonToRepJson (0 :: Int, msg)

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
            temp <- randomIdent 20
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

splitPath' :: Text -> [Text]
splitPath' = map T.pack . splitPath . T.unpack

randomIdent :: Int -> IO Text
randomIdent n = liftM (T.pack . map chr)
                  (MR.evalRandIO $ replicateM n rnd)
  where rnd = MR.getRandomR (65, 90)

-- | Retrieve user's playlist using various methods.
-- order: getparam -> cookie -> database -> new playlist
--
-- if "title" get parameter is set and no playlist is found, result is 404.
-- note: playlist with a title of "" is the default for any user
--
-- XXX: support hidden playlists?
getPlaylist :: Entity User -> Handler (Entity Playlist)
getPlaylist (Entity k v) = fromGetparam
  where
    fromGetparam = lookupGetParam "title" >>= \mt -> case mt of
        Just title -> runDB (getBy (UniquePlaylist k title)) >>= tryMaybe fromCookie 
        Nothing    -> fromCookie

    fromCookie = lookupCookie "playlist-title" >>= \mt' -> case mt' of
        Just title -> runDB (getBy (UniquePlaylist k title)) >>= tryMaybe fromDB
        Nothing    -> fromDB

    fromDB = case userCurrentplaylist v of
        Just plid -> liftM (fmap (Entity plid)) (runDB $ get plid) >>= tryMaybe fromDBDefault
        Nothing   -> fromDBDefault

    fromDBDefault = runDB (getBy $ UniquePlaylist k "") >>= tryMaybe (addDefaultPlaylist k)

clearPlaylist :: Playlist -> Playlist
clearPlaylist pl = pl { playlistElems = [] }

-- | add file OR every child of directory to playlist.
toPlaylist :: Playlist
           -> Text
           -> Text
           -> Handler (Bool, Playlist) -- (playlist changed?, maybe changed playlist)
toPlaylist pl area path = do
    t <- timeNow
    newElems <- findAll area path
    if null newElems
      then return (False, pl)
      else return (True, pl { playlistElems    = playlistElems pl ++ map ((,) area) newElems
                            , playlistModified = t })

-- | New titleless playlist for user with userId uid.
-- XXX: doesn't check for duplicates(!)
addDefaultPlaylist :: UserId -> Handler (Entity Playlist)
addDefaultPlaylist uid = do
    date <- timeNow
    let pl = Playlist "" uid [] date date
    k <- runDB $ insert pl
    return $ Entity k pl

-- | saves (replaces) an existing playlist. This is unsafe (replace) due to
-- unique constraints.
updatePlaylist :: PlaylistId -- ^ id of playlist to replace
               -> Playlist
               -> Handler Playlist -- ^ was update successful?
updatePlaylist plid pl = runDB (replace plid pl) >> return pl

-- | user specific playlist widget
widPlaylist :: Entity User -> Widget
widPlaylist (Entity _ uval) = do
    [main, actions, content, heading] <- replicateM 4 (lift newIdent)
    $(widgetFile "media-playlist")

-- | get playlists
myPlaylists :: Handler [Entity Playlist]
myPlaylists = runDB $ selectList ([] :: [Filter Playlist]) []

findAll :: Text -> Text -> Handler [Text]
findAll area path = do
    Entity k v <- runDB $ getBy404 $ UniqueFilenode area path
    if filenodeIsdir v
    then do
        childs <- liftM (map (filenodePath . entityVal)) $ runDB $ selectList [FilenodeParent ==. Just k] [Asc FilenodePath]
        liftM concat $ mapM (findAll area) childs
    else return [filenodePath v]


-- * Adminspace

getMediaAdminR :: Handler RepHtml
getMediaAdminR = do
   (widget, encType) <- generateFormPost adminForm
   defaultLayout $ do
      setTitle "media :: admin"
      $(widgetFile "media-admin")

postMediaAdminR :: Handler RepHtml
postMediaAdminR = do
   path <- gdir "anime"
   path' <- gdir "music"
   updateListing "anime" path
   updateListing "music" path'
   redirect MediaAdminR

adminForm :: Form (Bool, Bool)
adminForm = renderBootstrap $ (,)
   <$> areq boolField "Update all" (Just False)
   <*> areq boolField "Not used" (Just False)

-- | find paths and filestatuses recursively.
-- XXX: also find the argument?
find :: FilePath -> IO [(FilePath, FileStatus)]
find dir = do
   childs <- liftM (map (dir </>) . (\\ [".",".."])) (getDirectoryContents dir)
   stats  <- mapM getFileStatus childs
   let this = zip childs stats
   other <- mapM (find . fst) (filter (isDirectory . snd) this)
   return (this ++ concat other)

-- | 
updateListing :: Text
              -> FilePath
              -> Handler ()
updateListing area dir = do
    -- recursively find files and directories in `dir` along with properties
    infs <- liftM paths $ liftIO $ do
        stat   <- getFileStatus dir
        childs <- find dir
        return ( (dir,stat) : childs)

    -- delete entities not found in the filesystem XXX: disable/hide only?
    runDB $ deleteWhere [FilenodeArea ==. area, FilenodePath /<-. map fst infs]

    -- completely new entities
    unknown <- filterM (fmap isNothing . runDB . getBy . UniqueFilenode area . fst) infs
    mapM_ (runDB . insertNode) unknown
    -- XXX: find and update modified (newer than db) entities?

    -- Try to add parents to nodes
    orphans <- runDB $ selectList [ FilenodeArea   ==. area
                                  , FilenodeParent ==. Nothing] []
    mapM_ fixParent orphans
  where
    paths = map $ first (T.pack . normalise . drop (length dir + 1))

    insertNode (path,stat) = do
        parent <- getBy $ UniqueFilenode area (T.pack $ takeDirectory $ T.unpack path)
        insert =<< liftIO (toFilenode (dir </> T.unpack path) area (entityKey <$> parent) (T.pack $ normalise $ T.unpack path) stat)

    fixParent (Entity key val) = runDB $ do
        parent <- getBy $ UniqueFilenode area (T.pack $ takeDirectory $ T.unpack $ filenodePath val)
        update key [FilenodeParent =. fmap entityKey parent]

-- | 
toFilenode :: FilePath         -- ^ Real path to the file
           -> Text             -- ^ area
           -> Maybe FilenodeId -- ^ parent node
           -> Text             -- ^ path of the node
           -> FileStatus       -- ^ status of the node
           -> IO Filenode
toFilenode real area parent path stat = do
    details <- if not boolDir
      then fmap Just $ getDetails real
      else return Nothing
    return $ Filenode area
                      parent
                      boolDir
                      path
                      (prettyFilesize $ fileSize stat)
                      (posixSecondsToUTCTime $ realToFrac $ modificationTime stat)
                      details
  where
    boolDir = isDirectory stat

-- |
getDetails :: FilePath -> IO Text
getDetails fp = do
  (_, out, _) <- readProcessWithExitCode "mediainfo" [fp] ""
  return $ T.pack out

