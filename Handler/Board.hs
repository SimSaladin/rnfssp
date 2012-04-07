module Handler.Board where

import Import

getBoardR :: Handler RepHtml
getBoardR = do
    defaultLayout $ do
        setTitle "Welcome SS/board!"
        $(widgetFile "board")

postBoardR :: Handler RepHtml
postBoardR = do
    defaultLayout $ do
        setTitle "Thanks for posting!"
        $(widgetFile "board")
