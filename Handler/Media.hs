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
import qualified System.FilePath as F (joinPath)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath (combine)
import System.Posix.Files (getFileStatus, fileSize, modificationTime)

pathAnime :: FilePath
pathAnime = "/home/sim/anime"

getMediaR :: Handler RepHtml
getMediaR = do
    let fps = ["anime"] :: [FilePath]
    aent <- requireAuth
    defaultLayout $ do
        setTitle "Media"
        $(widgetFile "media")

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
          pc <- widgetToPageContent $(widgetFile "media")
          hamletToRepHtml [hamlet|^{pageBody pc}|]
        else do
          defaultLayout $ do
              setTitle "anime"
              $(widgetFile "media")

-- | Used to make changes to session (playlist etc.)
postMediaEntryR :: [FilePath] -> Handler RepHtml
postMediaEntryR fps = do
    redirect $ MediaEntryR fps


mediaDirWidget :: [FilePath] -> Widget
mediaDirWidget fps = do
    listId <- lift newIdent
    fp <- return $ case fps of
        ("anime":xs) -> combine pathAnime $ F.joinPath xs
        _ -> ""
    is_dir <- liftIO $ doesDirectoryExist fp
    is_file <- liftIO $ doesFileExist fp
    if is_dir
        then do
          files <- liftIO $ getDirectoryContents fp >>= \x -> return $ (\\) x [".",".."]
          urls <- return $ map (\x -> fps ++ [x]) files
          (sizes, dates) <- do
              stats <- liftIO $ mapM getFileStatus $ map (combine fp) files
              return (map fileSize stats, map modificationTime stats)
          let listing = zip4 files urls sizes dates
              nav = zip fps $ foldr (\x xs -> [[x]] ++ map ([x] ++) xs) [[]] fps
          $(widgetFile "media-listing")
        else if is_file
            then do
              [whamlet|viewing a file {not yet implemented!}|]
            else [whamlet|(No such file or directory was found)|]

mediaPlWidget :: UserId -> Widget
mediaPlWidget uid = do
    plId <- lift newIdent
    toWidget [lucius|
    |]
    toWidget [julius|
function load_playlist() {
    document.getElementById("#{plId}").innerHTML = "now loaded";
}

$("##{plId}").ready(load_playlist())
    |]
    toWidget [hamlet|
<h2>Playlist: #
    <span>
<div##{plId}>
    Loading playlist...
    |]

mediaPlayerWidget :: Widget
mediaPlayerWidget = do
    [whamlet|
    |]
