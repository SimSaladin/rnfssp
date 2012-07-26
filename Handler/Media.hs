{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Handler.Media
    ( getMediaR
    , postMediaR

    , getMediaDataR
    , postMediaDataR

    , getMediaAdminR
    , postMediaAdminR

    , getMediaPlaylistR
    , postMediaPlaylistR
    ) where

import Import
import qualified Data.Aeson.Types as A
import Data.List ((\\), init, last, head, tail)
import Data.Maybe (isJust)
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
   node' <- lift $ runDB $ getBy $ UniqueFilenode real
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
                let file     = takeFileName $ filenodePath val
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

-- | JSON-encoded playlists for ajax.
-- XXX: try cookie first instead of db if no playlist given?
-- TODO: support hidden playlists?
getMediaPlaylistR :: Handler RepJson
getMediaPlaylistR = do
  playlist <- getPlaylist404
  jsonToRepJson playlist

-- | creating, deleting, modifying playlists.
postMediaPlaylistR :: Handler RepJson
postMediaPlaylistR = do
  uent <- requireAuth
  mt <- lookupPostParam "title"
  ma <- lookupPostParam "action"
  case (mt, ma) of
    (Nothing,_) -> invalidArgs ["no title given"]
    (_,Nothing) -> invalidArgs ["no action given"]
    (Just title, Just action) -> do
        jsonToRepJson =<< case action of
          "insert" -> insertPlaylist title uent >>= \pl -> return $ array [pl]
          "delete" -> deletePlaylist title >>= return . Bool
          "update" -> parseJsonBody_ >>= updatePlaylist title >>= \pl -> return $ array [pl]

insertPlaylist :: Text -> Entity User -> Handler Playlist
insertPlaylist title (Entity uid _) = do
    date <- timeNow
    let pl = Playlist title uid [] date date
    mid <- runDB $ insertUnique pl
    case mid of
      Nothing -> invalidArgs ["Playlist with identical title already exists"]
      Just _  -> return pl

-- | true if deleted, false if didn't exist
deletePlaylist :: Text -> Handler Bool
deletePlaylist title = fmap isJust $
    runDB $ getBy upl >>= \ment -> deleteBy upl >> return ment
  where upl = UniquePlaylist title

updatePlaylist :: Text -> [Text] -> Handler Playlist
updatePlaylist title newElements = do
    Entity _ val <- runDB $ getBy404 $ UniquePlaylist title
    return val

widPlaylist :: Entity User -> Widget
widPlaylist (Entity _ uval) = do
  [main, actions, content] <- replicateM 3 (lift newIdent)
  $(widgetFile "media-playlist")

-- | getparam OR cookie OR database
getPlaylist404 :: Handler Playlist
getPlaylist404 = do
    mt <- lookupGetParam "title"
    mc <- lookupCookie "playlist-title"
    case mt of
      Just title -> get title
      Nothing -> do
          case mc of
            Just title -> get title
            Nothing -> do
                Entity _ val <- requireAuth
                case userCurrentplaylist val of
                  Just plid -> runDB $ get404 plid
                  Nothing -> notFound
  where get t = return . entityVal =<< (runDB $ getBy404 $ UniquePlaylist t)

-- | Lists every playlist and their information.
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

-- | update recursively
find :: FilePath -> IO [(FilePath, FileStatus)]
find dir = do
   childs <- getDirectoryContents dir >>= return . map (dir `combine`) . (\\ [".",".."])
   stats  <- mapM getFileStatus childs
   let this = zip childs stats
   other <- mapM (find . fst) (filter (isDirectory . snd) this)
   return (this ++ concat other)

-- | This function is very memory-inefficient!! TODO: do something about it.
updateListing :: String
              -> FilePath
              -> Handler ()
updateListing dest dir = do
   infs <- liftIO $ do
      stat <- getFileStatus dir
      childs <- find dir
      return ( (dir,stat) : childs)
   indb <- runDB $ selectList [FilenodeArea ==. dest] []

   let todelete = reverse $ map entityKey $ filter (not . existsFs) indb
       newNodes = filter (not . existsDb) infs
       existsDb (path,_) = path `elem` (map (filenodePath . entityVal) indb)
       existsFs ent = (filenodePath $ entityVal ent) `elem` (map fst infs)
   _ <- runDB $ do
      mapM delete todelete
      mapM insert' newNodes
      tocheck <- selectList [FilenodeParent ==. Nothing] []
      mapM_ checkParents tocheck

   return ()
   where
      toNode :: Maybe FilenodeId -> (FilePath, FileStatus) -> IO Filenode
      toNode parent (path, stat) = do
         details <- if guessFiletype path `elem` ["video", "audio"]
            then return $ Just "Calling mediainfo for every file easily timeouts the request, so it is disabled until a workaround is found" -- <$> readProcess "o" [path] ""
            else return Nothing
         let isDir = isDirectory stat
             size  = prettyFilesize $ fileSize stat
             time  = posixSecondsToUTCTime $ realToFrac $ modificationTime stat
         return $ Filenode dest parent isDir path size time details

      insert' (path, stat) = do
         parent <- selectFirst [FilenodePath ==. (takeDirectory path)] []
         node <- liftIO $ toNode (entityKey <$> parent) (path, stat)
         insert node

      checkParents ent = do
         let path = takeDirectory $ filenodePath $ entityVal ent
         parent <- selectFirst [FilenodePath ==. path] []
         update (entityKey ent) [FilenodeParent =. (fmap entityKey parent)]
