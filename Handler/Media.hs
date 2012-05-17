module Handler.Media
    ( getMediaR
    , getMediaEntryR
    , postMediaEntryR
    , pathAnime
    ) where

import Import
import Data.List ((\\))
import qualified System.FilePath as F (joinPath)
import System.Directory (getDirectoryContents, doesFileExist, doesDirectoryExist)
import System.FilePath (combine)

pathAnime :: FilePath
pathAnime = "/home/sim/anime"

getMediaR :: Handler RepHtml
getMediaR = do
    dirWidget <- liftIO $ mediaDirWidget [""]
    defaultLayout $ do
        setTitle "Media"
        $(widgetFile "media")

getMediaEntryR :: [FilePath] -> Handler RepHtml
getMediaEntryR fps = do
    dl <- lookupGetParam "download"
    case dl of
        Just d
            | d == "playlist" -> do
                setMessage "TODO: download playlist only"
                redirect $ MediaEntryR fps
            | d == "zip" -> do
                setMessage "TOOD: download as zip"
                redirect $ MediaEntryR fps
            | d == "torrent" -> do 
                setMessage "TODO: download as torrent"
                redirect $ MediaEntryR fps
            | otherwise -> invalidArgs ["Unknown download type"]
        _ -> do
            dirWidget <- liftIO $ mediaDirWidget fps
            bare <- lookupGetParam "bare"
            case bare of
                Just _ -> do
                    pc <- widgetToPageContent $(widgetFile "media")
                    hamletToRepHtml [hamlet|^{pageBody pc}|]
                Nothing -> do
                    defaultLayout $ do
                        setTitle "anime"
                        $(widgetFile "media")

-- |POST is used to make changes to session (playlist etc.)
postMediaEntryR :: [String] -> Handler RepHtml
postMediaEntryR fps = do
    redirect $ MediaEntryR fps

mediaDirWidget :: [FilePath] -> IO Widget
mediaDirWidget fps = do
    fp <- case fps of
        ("anime":xs) -> return $ combine pathAnime $ F.joinPath xs
        _ -> return ""
    is_dir <- doesDirectoryExist fp
    is_file <- doesFileExist fp

    listing <- case is_dir of
        True -> do
            list <- getDirectoryContents fp >>= \x -> return $ (\\) x [".",".."]
            return $ zip list $ map (\x -> fps ++ [x]) list
        False -> return []

    fileview <- case is_file of
        True -> return []
        False -> return []
    return $(widgetFile "media-listing")


