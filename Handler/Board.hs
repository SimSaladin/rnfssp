{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Board
    ( getBoardHomeR
    , getBoardR
    , postBoardR
    , getThreadR
    , postThreadR
    ) where

import Import
import Data.Time (getCurrentTime)
import Yesod.Form.Nic (nicHtmlField)

getBoardHomeR :: Handler RepHtml
getBoardHomeR = do
    defaultLayout $ do
        setTitle "Welcome SS/board!"
--        $(widgetFile "board")

getBoardR :: Text -> Handler RepHtml
getBoardR boardName = do
--    (formWidget, formEnctype) <- generateFormPost boardForm
--    let submission = Nothing :: Maybe (Text, Text, Text)
--        handlerName = "getBoardR" :: Text
    defaultLayout $ do
        setTitle "SS.board"
--        $(widgetFile "board")

postBoardR :: Text -> Handler RepHtml
postBoardR bname = do
--    ((result, formWidget), formEnctype) <- runFormPost boardForm
--    let handlerName = "postHomeR" :: Text
--        submission = case result of
--            FormSuccess res -> Just res
--            _ -> Nothing
    defaultLayout $ do
        setTitle "POST: SS/"
--        $(widgetFile "board")

getThreadR :: Text -> BoardpostId -> Handler RepHtml
getThreadR boardName boardpostId = do
    (op, rest) <- runDB $ do
        op <- get404 boardpostId
        --rest <-
        return (op, Nothing)
    defaultLayout $ do
        setTitle "SS.board.thread"

postThreadR :: Text -> BoardpostId -> Handler RepHtml
postThreadR bname tid = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    ((res, postWidget), encType) <- runFormPost (postForm (entityKey board))
--  thread <- runDB $ get tid
    defaultLayout $ do
        setTitle "POST: SS.board.thread"

postForm :: BoardId -> Form Boardpost
postForm bid = renderDivs $ Boardpost
       <$> pure bid
       <*> pure Nothing
       <*> aformM (liftIO getCurrentTime)
       <*> areq textField "Title" Nothing
       <*> aopt textField "Who" Nothing
       <*> aopt textareaField "Content" Nothing
