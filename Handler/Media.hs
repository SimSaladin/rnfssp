module Handler.Media
    ( getMediaR
    , getMediaDataR
    , getMediaEntryR
    , postMediaEntryR
    , pathAnime
    ) where

import Import
import Data.List ((\\), zip4)
import Data.Maybe (isJust, fromMaybe)
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import qualified System.FilePath as F (joinPath)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath (combine)
import System.Posix.Files (getFileStatus, fileSize, modificationTime)
import System.Posix (FileOffset)
import Text.Printf (printf)

pathAnime :: FilePath
pathAnime = "/home/sim/anime"
pathMusic :: FilePath
pathMusic = "/home/sim/music"

getMediaR :: Handler RepHtml
getMediaR = do
    mauth <- maybeAuth
    defaultLayout $ do
        setTitle "Media"
        $(widgetFile "media-home")

-- | Downloading files/data (user&js)
getMediaDataR :: Text -> Text -> Handler RepJson
getMediaDataR what which = do
    aent <- requireAuth
    case what of
        "playlist" -> do pl <- runDB $ getBy404 $ UniquePlaylist which
                         jsonToRepJson $ playlistToJson $ entityVal pl
        _          -> invalidArgs ["Invalid reply type: " `T.append` what]

-- | Either directory listing or file view. Whole page with widgets or if with
-- | get parameter `bare' set, only the listing/file view.
getMediaEntryR :: [FilePath] -> Handler RepHtml
getMediaEntryR fps = do
    aent <- requireAuth
    bare <- lookupGetParam "bare"
    if isJust bare
        then do
          pc <- widgetToPageContent $ browserWidget fps
          hamletToRepHtml [hamlet|^{pageBody pc}|]
        else do
          browserId <- newIdent
          defaultLayout $ do
              setTitle "anime"
              $(widgetFile "media")

-- | Used to make changes to session (playlist etc.)
postMediaEntryR :: [FilePath] -> Handler RepHtml
postMediaEntryR fps = do
    redirect $ MediaEntryR fps

browserWidget :: [FilePath] -> Widget
browserWidget fps = do
    nav <- return $ zip fps $ foldr (\x xs -> [[x]] ++ map ([x] ++) xs) [[]] fps
    fp <- return $ case fps of
        ("anime":xs) -> combine pathAnime $ F.joinPath xs
        ("music":xs) -> combine pathMusic $ F.joinPath xs
        _ -> ""
    is_dir <- liftIO $ doesDirectoryExist fp
    is_file <- liftIO $ doesFileExist fp
    listing <- if is_dir
        then do
          files <- liftIO $ getDirectoryContents fp >>= \x -> return (x \\ [".",".."])
          urls <- return $ map (\x -> fps ++ [x]) files
          (sizes, dates) <- do
              stats <- liftIO $ mapM getFileStatus $ map (combine fp) files
              return (map (printSize . fileSize) stats, map modificationTime stats)
          return $ zip4 files urls sizes dates
        else if is_file
            then return []
            else return []
    $(widgetFile "media-listing")

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
        in $(widgetFile "media-playlist")

-- | encode playlist's title, owner and elements in a json object
playlistToJson :: Playlist -> Value
playlistToJson pl = object [ ("title", title)
                           , ("elems", elems)
                           ] where title = String $ playlistTitle pl
                                   elems = array $ playlistElems pl

-- | Lists every playlist and show information about them
playlistsWidget :: Widget
playlistsWidget = do
    pls <- lift $ runDB $ do
        pls <- selectList ([] :: [Filter Playlist]) []
        return pls
    toWidget [hamlet|playlist listing: not yet implemented|]

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

