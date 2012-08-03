{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, DoAndIfThenElse #-}
module Handler.Media
    ( getMediaR
    , postMediaR
    , getMediaServeR

    , getMediaAdminR
    , postMediaAdminR

    , postMediaPlaylistR
    ) where

import Import
import qualified Data.Aeson.Types as A
import Data.List ((\\), init, last, head, tail)
import Data.Maybe (isJust, isNothing)
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import Control.Arrow (first)
import Control.Monad
import qualified Data.Text as T
import qualified System.FilePath as F (joinPath)
import System.Locale (defaultTimeLocale)
import System.Directory (getDirectoryContents)
import System.FilePath -- (combine)
import System.Posix.Files -- (getFileStatus, fileSize, modificationTime, isDirectory)
import System.Posix (FileOffset)
import System.Process (readProcessWithExitCode)
import Text.Printf (printf)
import Yesod.Json
import Yesod.Default.Config (appExtra)

instance A.ToJSON Playlist where
    toJSON (Playlist title owner elems create mod) = object
        [ "title" .= title
        , "owner" .= owner
        , "elems" .= elems
        , "created" .= create
        , "modified" .= mod
        ]

-- * Confs & Utilities

viewable :: [Text]
viewable = ["audio", "video"]

isdir :: Text -> Bool
isdir = (==) "directory"

-- | File size prettified
prettyFilesize :: FileOffset -> Text
prettyFilesize off = T.pack $ toprint off
  where
    f n = printf "%.0f" (fromIntegral off / n :: Float)
    toprint x | x >= lT   = f lT ++ "T"
              | x >= lG   = f lG ++ "G"
              | x >= lM   = f lM ++ "M"
              | x >= lK   = f lK ++ "K"
              | x >= lB   = f lB ++ "B"
              | otherwise = "n/a"
        where
            [lB,lK,lM,lG,lT] = scanl (*) 1 $ take 4 $ repeat 1024

guessFiletype :: FilePath -> Text
guessFiletype fp = if ext `elem` (map ('.':) ["mkv","avi","sfv","ogm","mp4"])
   then "video"
   else if ext `elem` (map ('.':) ["flac","mid","mp3","ogg","tak","tif","tta","wav","wma","wv"])
      then "audio"
      else "unknown"
   where ext = takeExtension fp

-- | Get real filepath for @which@ master directory.
gdir :: Text -> Handler FilePath
gdir which = do
   master <- getYesod
   let set = appExtra $ settings master
      in return $ case which of
            "anime" -> extraDirAnime set
            "music" -> extraDirMusic set
            -- XXX: fails for any other runtime!

widgetOnly :: Widget -> Handler RepHtml
widgetOnly w = widgetToPageContent w >>= \pc -> hamletToRepHtml [hamlet|^{pageBody pc}|]

-- | convienience
timeNow :: Handler UTCTime
timeNow = liftIO getCurrentTime

toPath :: [Text] -> Text
toPath = T.intercalate "/"

tryMaybe :: Monad m => m a -> Maybe a -> m a
tryMaybe or this = case this of
    Just a -> return a
    Nothing -> or

if' :: Bool -> a -> a -> a
if' cond th el = if cond then th else el


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

getMediaServeR :: Text   -- ^ type of download:
               -> Text   -- ^ file area
               -> [Text] -- ^ path for the file
               -> Handler RepJson
getMediaServeR _ _ []        = invalidArgs ["Invalid download target"]
getMediaServeR t area path = do
    uid  <- requireAuthId
    Entity key val <- runDB $ getBy404 $ UniqueFilenode area (toPath path)
    if filenodeIsdir val
      then invalidArgs ["Downloading directories is not supported"]
      else do
          now <- timeNow
          fsRoot <- gdir area
          let fsPath = fsRoot </> (T.unpack $ filenodePath val)
          runDB $ insert $ LogDownload uid now key
          case t of
            "force" -> sendFile "application/force-download" fsPath
            "auto"  -> sendFile "" fsPath
            _       -> invalidArgs ["Invalid/unsupported download type"]
      

-- * Browser

-- | the <table> element only
browserTable :: [Text] -> Handler RepHtml
browserTable fps = do
    rauth <- requireAuth
    pc <- widgetToPageContent $ browserViewWidget fps
    hamletToRepHtml [hamlet|^{pageBody pc}|]

-- | The driver for the browser
browserWidget :: [Text] -> Widget
browserWidget fps = do
    browserId <- lift newIdent
    $(widgetFile "browser-driver")
  where
    active = if null fps then "" else head fps
    sections = map (\(sect, icon) -> (active == sect, MediaR [sect], sect, icon)) browsable

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
        val <- map entityVal nodes
        let file     = takeFileName $ T.unpack $ filenodePath val
            filetype = if filenodeIsdir val
                         then "directory"
                         else guessFiletype file
            fps      = path ++ [T.pack file]
            size     = filenodeSize val
            modified = formatTime defaultTimeLocale "%d.%m -%y" $ filenodeModTime val
            details  = filenodeDetails val
            in return (file, filetype, fps, area : fps, size, modified)
    $(widgetFile "browser-bare")
  where
    nav = zip (area:path) (foldr (\x xs -> [[x]] ++ map ([x] ++) xs) [[]] (area:path))


-- * Playing & Playlists

instance A.FromJSON Playlist where
   parseJSON (Object v) = Playlist
                           <$> v A..: "title"
                           <*> v A..: "owner"
                           <*> v A..: "elems"
                           <*> v A..: "created"
                           <*> v A..: "modified"

-- | action: "select" returns current or creates a new playlist
--           "push" adds an filenode and its children to current playlist and
--                  saves it if possible.
postMediaPlaylistR :: Text -> Handler RepJson
postMediaPlaylistR action = do
    uent <- requireAuth
    Entity plk pl <- getPlaylist (uent)
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
        Just plid -> runDB (get plid) >>= return . fmap (Entity plid) >>= tryMaybe fromDBDefault
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

-- | Lists every playlist and their information.
-- XXX: implement hidden/private playlists?
widPlaylists :: Widget
widPlaylists = do
    pls <- lift $ runDB $ do
        pls <- selectList ([] :: [Filter Playlist]) []
        return pls
    toWidget [hamlet|playlist listing: not yet implemented|]

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
   childs <- getDirectoryContents dir >>= return . map (dir `combine`) . (\\ [".",".."])
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
        update key [FilenodeParent =. (fmap entityKey parent)]

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
  (c, out, err) <- readProcessWithExitCode "mediainfo" [fp] ""
  return $ T.pack out

