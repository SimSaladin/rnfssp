module Handler.Media
    ( getMediaR
    , postMediaR
    , getMediaDataR
    , postMediaDataR
--    , getMediaEntryR
--    , postMediaEntryR
    , pathAnime
    ) where

import Import
import Data.List ((\\), zip4, init, last)
import Data.Maybe (isJust, fromMaybe)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX (POSIXTime, posixSecondsToUTCTime)
import qualified Data.Text as T
import qualified System.FilePath as F (joinPath)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath (combine)
import System.Posix.Files (getFileStatus, fileSize, modificationTime)
import System.Posix (FileOffset)
import Text.Printf (printf)

pathAnime :: FilePath
pathAnime = "/home/media/anime"
pathMusic :: FilePath
pathMusic = "/home/media/music"

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

bareLs :: [FilePath] -> Handler RepHtml
bareLs fps = do
    rauth <- requireAuth
    pc <- widgetToPageContent $ browserViewWidget fps
    hamletToRepHtml [hamlet|^{pageBody pc}|]

-- | Downloading files/data (user&js)
getMediaDataR :: Text -> Text -> [String] -> Handler RepJson
getMediaDataR what which file = do
    aent <- requireAuth
    case what of
        "playlist" -> do pl <- runDB $ getBy404 $ UniquePlaylist which
                         jsonToRepJson $ playlistToJson $ entityVal pl
        "download" -> sendFile "" $ toRealPath file
        _          -> invalidArgs ["Invalid reply type: " `T.append` what]

postMediaDataR :: Text -> Text -> [String] -> Handler RepJson
postMediaDataR _ _ _ = do
    r <- parseJsonBody_
    liftIO $ putStrLn "teh value:"
    liftIO $ putStrLn $ show (r :: Value)
    jsonToRepJson r

browserWidget :: [FilePath] -> Widget
browserWidget fps = do
    browserId <- lift newIdent
    $(widgetFile "browser-driver")

-- | requires the browser script already on the page
browserViewWidget :: [FilePath] -> Widget
browserViewWidget fps = do
    let nav = zip fps $ foldr (\x xs -> [[x]] ++ map ([x] ++) xs) [[]] fps
        fp  = toRealPath fps
    is_dir <- liftIO $ doesDirectoryExist fp
    is_file <- liftIO $ doesFileExist fp
    listing <- if is_dir
        then do
          files <- liftIO $ getDirectoryContents fp >>= \x -> return (x \\ [".",".."])
          stats <- liftIO $ mapM getFileStatus $ map (combine fp) files
          let urls  = map (\x -> fps ++ [x]) files
              sizes = map (printSize . fileSize) stats
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

-- | encode playlist's title, owner and elements in a json object
playlistToJson :: Playlist -> Value
playlistToJson pl = object [ ("title", title)
                           , ("elems", elems)
                           ] where title = String $ playlistTitle pl
                                   elems = array $ playlistElems pl

toRealPath :: [FilePath] -> FilePath
toRealPath fps = combine root $ F.joinPath xs
    where (root, xs) = case fps of
            ("anime":xs) -> (pathAnime,xs)
            ("music":xs) -> (pathMusic,xs)
            _ -> ("",[])

printSize :: FileOffset -> Text
printSize x = T.pack $ toprint x where
    f n = printf "%.0f" (fromIntegral x / n :: Float)
    toprint x | x >= lT   = f lT ++ "T"
              | x >= lG   = f lG ++ "G"
              | x >= lM   = f lM ++ "M"
              | x >= lK   = f lK ++ "K"
              | x >= lB   = f lB ++ "B"
              | otherwise = "n/a"
        where
            [lB,lK,lM,lG,lT] = scanl (*) 1 $ take 4 $ repeat 1024

