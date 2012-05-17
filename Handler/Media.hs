module Handler.Media
    ( getMediaR
    , getMediaEntryR
    , postMediaEntryR
    ) where

import Import
import qualified System.FilePath as F (joinPath)

animeFilePath :: FilePath
animeFilePath = "/home/sim/anime"

getMediaR :: Handler RepHtml
getMediaR = do
    let dirWidget = mediaDirWidget ["/"]
    defaultLayout $ do
        setTitle "Media"
        $(widgetFile "media")

getMediaEntryR :: [FilePath] -> Handler RepHtml
getMediaEntryR fps = do
    dl <- lookupGetParam "download"
    case dl of
        Just d
            | d == "playlist" -> do
                setMessage "TODO: download playlist"
                redirect $ MediaEntryR fps
            | d == "zip" -> do
                setMessage "TOOD: download as a zip"
                redirect $ MediaEntryR fps
            | d == "torrent" -> do 
                setMessage "TODO: download in torrent"
                redirect $ MediaEntryR fps
            | otherwise -> invalidArgs ["Unknown download type"]
        _ -> do
            let dirWidget = mediaDirWidget fps
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

mediaDirWidget :: [FilePath] -> Widget
mediaDirWidget fps = do
    let fp = F.joinPath fps
    $(widgetFile "media-listing")
