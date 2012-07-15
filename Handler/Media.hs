module Handler.Media
    ( getMediaR
    , postMediaR
    , getMediaDataR
    , postMediaDataR
    , getMediaAdminR
    , postMediaAdminR
    ) where

import Import
import Data.List ((\\), zip4, init, last, head, tail)
import Data.Maybe (isJust, fromMaybe)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import Data.Time.Format (formatTime)
import qualified Data.Text as T
import qualified System.FilePath as F (joinPath)
import System.Locale (defaultTimeLocale)
import System.Process (readProcess)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath -- (combine)
import System.Posix.Files -- (getFileStatus, fileSize, modificationTime, isDirectory)
import System.Posix (FileOffset)
import Text.Printf (printf)
import Yesod.Default.Config (appExtra)

viewable = ["audio", "video"]
isdir = (==) "directory"

gdir :: String -> Handler FilePath
gdir which = do
   master <- getYesod
   let set = appExtra $ settings master
      in return $ case which of
            "anime" -> extraDirAnime set
            "music" -> extraDirMusic set

-- | The root and heart of media section. Loads browser, playlist and other
--   possibly widgets. 
getMediaR :: [FilePath] -> Handler RepHtml
getMediaR fps = do
    mauth <- maybeAuth
    bare <- lookupGetParam "bare"
    case bare of
        Just _ -> bareLs fps
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
      "playlist" -> do
         pl <- runDB $ getBy404 $ UniquePlaylist which
         jsonToRepJson $ playlistToJson $ entityVal pl

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

-- | the <table> element only
bareLs :: [FilePath] -> Handler RepHtml
bareLs fps = do
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

guessFiletype :: FilePath -> Text
guessFiletype fp = if ext `elem` (map ('.':) ["mkv","avi","sfv","ogm","mp4"])
   then "video"
   else if ext `elem` (map ('.':) ["flac","mid","mp3","ogg","tak","tif","tta","wav","wma","wv"])
      then "audio"
      else "unknown"
   where ext = takeExtension fp

-- | a widget for a single playlist/player
playerWidget :: Entity User -> Widget
playerWidget uent = do
    idPlaylist <- lift newIdent
    idPlayer <- lift newIdent
    maybePl <- case userCurrentplaylist $ entityVal uent of
        Just plid -> lift . runDB $ get plid
        Nothing   -> return Nothing
    pl <- case maybePl of
        Just p  -> return p
        Nothing -> liftIO getCurrentTime >>= \t -> return
            $ Playlist "" (entityKey uent) [] t t
    let elems = playlistElems pl
        title = playlistTitle pl
        rout  = MediaDataR "playlist" "a" [""]
        in $(widgetFile "media-playlist")

-- | Lists every playlist and show information about them
playlistsWidget :: Widget
playlistsWidget = do
    pls <- lift $ runDB $ do
        pls <- selectList ([] :: [Filter Playlist]) []
        return pls
    toWidget [hamlet|playlist listing: not yet implemented|]

-- | encode playlist's title, owner and elements in a json object.
playlistToJson :: Playlist -> Value
playlistToJson pl = object [ ("title", title)
                           , ("elems", elems)
                           ] where title = String $ playlistTitle pl
                                   elems = array $ playlistElems pl

-- | File size prettified
prettyFilesize :: FileOffset -> Text
prettyFilesize x = T.pack $ toprint x where
    f n = printf "%.0f" (fromIntegral x / n :: Float)
    toprint x | x >= lT   = f lT ++ "T"
              | x >= lG   = f lG ++ "G"
              | x >= lM   = f lM ++ "M"
              | x >= lK   = f lK ++ "K"
              | x >= lB   = f lB ++ "B"
              | otherwise = "n/a"
        where
            [lB,lK,lM,lG,lT] = scanl (*) 1 $ take 4 $ repeat 1024

-- | update recursively
find :: FilePath -> IO [(FilePath, FileStatus)]
find dir = do
   childs <- getDirectoryContents dir >>= return . map (dir `combine`) . (\\ [".",".."])
   stats  <- mapM getFileStatus childs
   let this = zip childs stats
   other <- mapM (find . fst) (filter (isDirectory . snd) this)
   return (this ++ concat other)

updateListing :: String -> FilePath -> Handler ()
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
   runDB $ do
      mapM delete todelete
      mapM insert' newNodes
      tocheck <- selectList [FilenodeParent ==. Nothing] []
      mapM_ checkParents tocheck

   return ()
   where
      toNode :: Maybe FilenodeId -> (FilePath, FileStatus) -> IO Filenode
      toNode parent (path, stat) = do
         details <- if guessFiletype path `elem` ["video", "audio"]
            then Just <$> readProcess "mediainfo" [path] ""
            else return Nothing
         let isdir = isDirectory stat
             size  = prettyFilesize $ fileSize stat
             time  = posixSecondsToUTCTime $ realToFrac $ modificationTime stat
         return $ Filenode dest parent isdir path size time details

      insert' (path, stat) = do
         parent <- selectFirst [FilenodePath ==. (takeDirectory path)] []
         node <- liftIO $ toNode (entityKey <$> parent) (path, stat)
         insert node

      checkParents ent = do
         let path = takeDirectory $ filenodePath $ entityVal ent
         parent <- selectFirst [FilenodePath ==. path] []
         update (entityKey ent) [FilenodeParent =. (fmap entityKey parent)]

