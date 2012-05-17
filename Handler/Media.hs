module Handler.Media
    ( getMediaR
    , getMediaEntryR
    , getMediaAnimeFileR
    ) where

import Import

getMediaR :: Handler RepHtml
getMediaR = do
    defaultLayout $ do
        setTitle "Media"
        $(widgetFile "media")

getMediaEntryR :: [Text] -> Handler RepHtml
getMediaEntryR args = do
    defaultLayout $ do
        $(widgetFile "media")

getMediaAnimeFileR :: [Text] -> Handler RepHtml
getMediaAnimeFileR args = do
    defaultLayout $ do
        $(widgetFile "media")

mediaDirWidget :: Widget
mediaDirWidget = do
    $(widgetFile "media-listing")
