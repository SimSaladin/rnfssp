module Handler.Media
    ( getMediaR
    , getMediaDataR
    , getMediaEntryR
    , postMediaEntryR
    , pathAnime
    ) where

import Import
import Data.List ((\\), zip4)
import Data.Maybe (isJust)
import Data.Text (pack)
import qualified System.FilePath as F (joinPath)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath (combine)
import System.Posix.Files (getFileStatus, fileSize, modificationTime)
import System.Posix (FileOffset)
import Text.Printf (printf, PrintfArg)

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
getMediaDataR :: Text -> Handler RepJson
getMediaDataR toget = do
    aent <- requireAuth
    case toget of
        "playlist" -> do
            jsonToRepJson $ array ["aoeu" :: Text] -- FIXME/TODO
        _ -> invalidArgs ["Target not found"]

-- | Either directory listing or file view. Whole page with widgets or if with
-- | get parameter `bare' set, only the listing/file view.
getMediaEntryR :: [FilePath] -> Handler RepHtml
getMediaEntryR fps = do
    aent <- requireAuth
    bare <- lookupGetParam "bare"
    if isJust bare
        then do
          pc <- widgetToPageContent $ mediaDirWidget fps
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


mediaDirWidget :: [FilePath] -> Widget
mediaDirWidget fps = do
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

mediaPlWidget :: UserId -> Widget
mediaPlWidget uid = do
    
    plId <- lift newIdent
    toWidget [lucius|
    |]
    toWidget [julius|
function load_playlist() {
    document.getElementById("#{plId}").innerHTML = "now loaded";
}
function upload_playlist() {
    
}

$("##{plId}").ready(load_playlist())
    |]
    toWidget [hamlet|
<div.page-element>
    <h2>Playlist: #
        <span>
    <div##{plId}>
        Loading playlist...
    |]

mediaPlayerWidget :: Widget
mediaPlayerWidget = do
    [whamlet|
    |]

printSize :: FileOffset -> Text
printSize x | x >= 1024 ^ 4 = pack $ printf "%.0f" (fromIntegral x / 1024 ^ 4 :: Float) ++ "T"
            | x >= 1024 ^ 3 = pack $ printf "%.0f" (fromIntegral x / 1024 ^ 3 :: Float) ++ "G"
            | x >= 1024 ^ 2 = pack $ printf "%.0f" (fromIntegral x / 1024 ^ 2 :: Float) ++ "M"
            | x >= 1024     = pack $ printf "%.0f" (fromIntegral x / 1024 :: Float) ++ "K"
            | x >= 0        = pack $ printf "%.0f" (fromIntegral x :: Float) ++ "B"
            | otherwise     = "n/a"
---printSize x = return x | x >= 1024 ^ 4
--    >>= \(n, m) -> pack $ printf "%.2f" (fromIntegral x / 1024 ^ n :: Float) ++ m

