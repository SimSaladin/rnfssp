{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Handler.Media
    ( getMediaR
    , postMediaR

    , getMediaDataR
    , postMediaDataR

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
gdir :: String -> Handler FilePath
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

toPath :: [String] -> String
toPath = foldl1 (\x y -> x ++ "/" ++ y)

-- * Userspace / General

-- | The root and heart of media section. Loads browser, playlist and other
--   possibly widgets. 
getMediaR :: [FilePath] -> Handler RepHtml
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
postMediaR :: [FilePath] -> Handler RepHtml
postMediaR fps = do
    setMessage "This is not yet supported"
    redirect $ MediaR fps

-- | Downloading files/data (user&js)
getMediaDataR :: Text     -- ^ one of `playlist`, `download`
              -> Text     -- ^ for playlist, the title. for download, not used.
              -> [String] -- ^ for download: specifies filepath
              -> Handler RepJson
getMediaDataR what which fp = do
   entid <- requireAuthId

   case what of
      -- TODO: use "Header Range" and sendResponse (type, toContent {ByteString})
      -- ... allows the download to be continued?
      "download" -> if null fp
         then notFound
         else do
            root <- gdir $ head fp
            let path = root </> (F.joinPath $ tail fp)

            time <- liftIO getCurrentTime

            _ <- runDB $ insert $ LogDownload entid time path
            case which of
               "force" -> sendFile "application/force-download" path
               "auto"  -> sendFile "" path
               _       -> invalidArgs ["Invalid download argument"]

      _ -> invalidArgs ["Invalid reply type: " `T.append` what]

postMediaDataR :: Text -> Text -> [String] -> Handler RepJson
postMediaDataR _ _ _ = do
    r <- parseJsonBody_
    liftIO $ putStrLn "teh value:"
    liftIO $ putStrLn $ show (r :: Value)
    jsonToRepJson r

-- * Browser

-- | the <table> element only
browserTable :: [FilePath] -> Handler RepHtml
browserTable fps = do
    rauth <- requireAuth
    pc <- widgetToPageContent $ browserViewWidget fps
    hamletToRepHtml [hamlet|^{pageBody pc}|]

-- | The driver for the browser
browserWidget :: [FilePath] -> Widget
browserWidget fps = do
    browserId <- lift newIdent
    $(widgetFile "browser-driver")

-- | requires the browser script already on the page
browserViewWidget :: [FilePath] -> Widget
browserViewWidget fps = do
   root <- lift $ gdir $ head fps
   let real = root </> (F.joinPath $ tail fps)
       nav  = zip fps (foldr (\x xs -> [[x]] ++ map ([x] ++) xs) [[]] fps)
   liftIO $ print (root, real)
   node' <- lift $ runDB $ getBy $ UniqueFilenode (T.pack real)
   case node' of
      Nothing -> let is_dir  = False
                     is_file = False
                     details = Nothing :: Maybe String
                     listing = [] :: [(String, Text, [String], Text, Text)]
                     in $(widgetFile "browser-bare")

      Just node -> let val     = entityVal node
                       is_dir  = filenodeIsdir val
                       is_file = not is_dir
                       details = filenodeDetails val
                       in do
         nodes <- lift $ if is_dir
            then runDB $ selectList [FilenodeParent ==. (Just $ entityKey node)] [Asc FilenodePath]
            else return []
         let listing = do
                val <- map entityVal nodes
                let file     = takeFileName $ T.unpack $ filenodePath val
                    filetype = if filenodeIsdir val
                                 then "directory"
                                 else guessFiletype file
                    split    = fps ++ [file]
                    size     = filenodeSize val
                    modified = formatTime defaultTimeLocale "%d.%m -%y" $ filenodeModTime val
                    details  = filenodeDetails val
                    in return (file, filetype, split, size, modified)
         $(widgetFile "browser-bare")

-- * Playing & Playlists

-- | creating, deleting, modifying playlists.
postMediaPlaylistR :: Text -> Handler RepJson
postMediaPlaylistR action = do
  uent <- requireAuth
  mtitle <- lookupPostParam "title"
  jsonToRepJson =<< case mtitle of
    Nothing -> invalidArgs ["no title given"]
    Just title -> case action of
      "select" -> getPlaylist404 >>= return . array . return
      "create" -> createPlaylist title uent >>= return . array . return
      "delete" -> deletePlaylist title >>= return . Bool
      "update" -> updatePlaylist title [] >>= return . array . return

-- | completely new playlist.
createPlaylist :: Text -> Entity User -> Handler Playlist
createPlaylist title (Entity uid _) = do
    date <- timeNow
    let pl = Playlist title uid [] date date
    mid <- runDB $ insertUnique pl
    case mid of
      Nothing -> invalidArgs ["Playlist with identical title already exists"]
      Just _  -> return pl

-- |
deletePlaylist :: Text
               -> Handler Bool -- ^ true if deleted, false if never existed XXX: unmodifiable playlists?
deletePlaylist title = fmap isJust $
    runDB $ getBy upl >>= \ment -> deleteBy upl >> return ment
  where upl = UniquePlaylist title

updatePlaylist :: Text -> [Text] -> Handler Playlist
updatePlaylist title newElements = do
    Entity _ val <- runDB $ getBy404 $ UniquePlaylist title
    return val

-- | user specific playlist widget
widPlaylist :: Entity User -> Widget
widPlaylist (Entity _ uval) = do
    [main, actions, content] <- replicateM 3 (lift newIdent)
    $(widgetFile "media-playlist")

-- | getparam OR cookie OR database
-- XXX: try cookie first instead of db if no playlist given?
-- TODO: support hidden playlists?
getPlaylist404 :: Handler Playlist
getPlaylist404 = do
    mt <- lookupGetParam "title"
    mc <- lookupCookie "playlist-title"
    case mt of
      Just title -> get title
      Nothing -> case mc of
        Just title -> get title
        Nothing -> do
            Entity _ val <- requireAuth
            case userCurrentplaylist val of
              Just plid -> runDB $ get404 plid
              Nothing -> notFound
  where get t = return . entityVal =<< (runDB $ getBy404 $ UniquePlaylist t)

-- | Lists every playlist and their information.
-- XXX: implement hidden/private playlists?
widPlaylists :: Widget
widPlaylists = do
    pls <- lift $ runDB $ do
        pls <- selectList ([] :: [Filter Playlist]) []
        return pls
    toWidget [hamlet|playlist listing: not yet implemented|]

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

-- | This function is very memory-inefficient!! TODO: do something about it.
updateListing :: Text
              -> FilePath
              -> Handler ()
updateListing area dir = do
    infs' <- liftIO $ do -- find every file in filesystem along with properties
        stat   <- getFileStatus dir
        childs <- find dir
        return ( (dir,stat) : childs)
    let infs = map (\(x,y) -> (T.pack x, y)) infs'
        paths = map fst infs

    -- delete entities not found in the filesystem
    -- XXX: disable/hide only?
    runDB $ deleteWhere [FilenodeArea ==. area, FilenodePath /<-. paths]

    -- insert completely new entities
    unknown <- filterM (fmap isNothing . runDB . getBy . UniqueFilenode . fst) infs
    mapM (runDB . insertNode) unknown

    -- XXX: find and update modified entities?

    liftIO $ mapM_ (putStrLn . T.unpack . fst) (take 100 unknown) -- debugging only?
    return ()
   where
    insertNode (path,stat) = do
        parent <- selectFirst [FilenodePath ==. (T.pack $ takeDirectory $ T.unpack path)] []
        insert =<< liftIO (toFilenode area (entityKey <$> parent) path stat)

-- | 
toFilenode :: Text             -- ^ area
           -> Maybe FilenodeId -- ^ parent node
           -> Text             -- ^ path of the node
           -> FileStatus       -- ^ status of the node
           -> IO Filenode
toFilenode area parent path stat = do
    details <- if not boolDir then fmap Just $ getDetails $ T.unpack path else return Nothing
    return $ Filenode area
                      parent
                      boolDir
                      path
                      (prettyFilesize $ fileSize stat)
                      (posixSecondsToUTCTime $ realToFrac $ modificationTime stat)
                      details
  where
    boolDir = isDirectory stat

getDetails :: FilePath -> IO Text
getDetails fp = readProcessWithExitCode "mediainfo" [fp] "" >>= \(_,x,_) -> return $ T.pack x
