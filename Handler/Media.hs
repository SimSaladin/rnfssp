module Handler.Media
    ( getMediaR
    , postMediaR
    , getMediaDataR
    , postMediaDataR
    , getMediaAdminR
    , postMediaAdminR
    ) where

import Import
import Data.List ((\\), zip4, init, last)
import Data.Maybe (isJust, fromMaybe)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import qualified Data.Text as T
import qualified System.FilePath as F (joinPath)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath -- (combine)
import System.Posix.Files -- (getFileStatus, fileSize, modificationTime, isDirectory)
import System.Posix (FileOffset)
import Text.Printf (printf)
import Yesod.Default.Config (appExtra)

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
getMediaDataR :: Text -> Text -> [String] -> Handler RepJson
getMediaDataR what which file = do
    rp <- getYesod >>= \x -> return $ toLocalPath x file
    entid <- requireAuthId
    case what of
        "playlist" -> do pl <- runDB $ getBy404 $ UniquePlaylist which
                         jsonToRepJson $ playlistToJson $ entityVal pl
        "download" -> do time <- liftIO getCurrentTime
                         _ <- runDB $ insert $ LogDownload entid time rp
                         -- TODO: use Header Range and sendResponse (type, toContent {ByteString})
                         sendFile "" rp
        _          -> invalidArgs ["Invalid reply type: " `T.append` what]

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
   updateListing "anime" path
   redirect MediaAdminR

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
   infs <- liftIO $ find dir
   indb <- runDB $ selectList ([] :: [Filter Filenode]) []

   let todelete = map entityKey $ filter (not . existsFs) indb
       newNodes = filter (not . existsDb) infs
       existsDb (path,_) = path `elem` (map (filenodePath . entityVal) indb)
       existsFs ent = (filenodePath $ entityVal ent) `elem` (map fst infs)

   liftIO $ print $ show todelete

   _ <- runDB $ do
      mapM delete todelete
      mapM insert' newNodes

   return ()

   where
      toNode parent (path, stat) = if isDirectory stat
         then Filenode parent True path
         else Filenode parent False path

      insert' (path, stat) = selectFirst
            [FilenodePath ==. (takeDirectory path)] [] >>= \parent ->
         insert $ Filenode (entityKey <$> parent) (isDirectory stat) path

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

-- |
browserWidget :: [FilePath] -> Widget
browserWidget fps = do
    browserId <- lift newIdent
    $(widgetFile "browser-driver")

-- | requires the browser script already on the page
browserViewWidget :: [FilePath] -> Widget
browserViewWidget fps = do
   fp <- lift getYesod >>= \x -> return $ toLocalPath x fps
   let nav = zip fps $ foldr (\x xs -> [[x]] ++ map ([x] ++) xs) [[]] fps
   is_dir <- liftIO $ doesDirectoryExist fp
   is_file <- liftIO $ doesFileExist fp
   listing <- if is_dir
       then do
         files <- liftIO $ getDirectoryContents fp >>= \x -> return (x \\ [".",".."])
         stats <- liftIO $ mapM getFileStatus $ map (combine fp) files
         let urls  = map (\x -> fps ++ [x]) files
             sizes = map (prettyFilesize . fileSize) stats
             dates = map (\x -> posixSecondsToUTCTime (realToFrac $ modificationTime x)) stats
         return $ zip4 files urls sizes dates
       else return []
   $(widgetFile "browser-bare")

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
        rout  = MediaDataR "a" "a" [""]
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

-- |
toLocalPath :: App -> [FilePath] -> FilePath
toLocalPath master fps = do
   let (root, xs) = case fps of
            ("anime":xs) -> (extraDirAnime $ appExtra $ settings master,xs)
            ("music":xs) -> (extraDirMusic $ appExtra $ settings master,xs)
            _ -> ("",[])
   combine root $ F.joinPath xs

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

