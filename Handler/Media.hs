module Handler.Media
    ( getMediaR
    ) where

import Import

getMediaR :: Handler RepHtml
getMediaR = do
    defaultLayout $ do
        setTitle "Media"
        $(widgetFile "media")

getMediaDirectory :: Widget
getMediaDirectory = do
    $(widgetFile "media-listing")
