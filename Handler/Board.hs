module Handler.Board
    ( getBoardHomeR
    , getBoardR
    , postBoardR
    , getThreadR
    , postThreadR
    ) where

import Import
import Data.Text (append)
import Data.Time (getCurrentTime)
-- import Yesod.Form.Nic (nicHtmlField)

-- /board/
getBoardHomeR :: Handler RepHtml
getBoardHomeR = do
    boards <- runDB $ selectList ([] :: [Filter Board]) []
    defaultLayout $ do
        setTitle "Lauta"
        $(widgetFile "board-home")

-- /board/[b]/
getBoardR :: Text -> Handler RepHtml
getBoardR bname = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    (formWidget, encType) <- generateFormPost (postForm (entityKey board))
    let submission = Nothing :: Maybe Boardpost
    defaultLayout $ do
        setTitle $ toHtml $ "/" `append` bname `append` "/ | Lauta"
        $(widgetFile "board")

postBoardR :: Text -> Handler RepHtml
postBoardR bname = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    ((result, formWidget), encType) <- runFormPost (postForm (entityKey board))
    let submission = case result of
           FormSuccess res -> Just res
           _ -> Nothing
    defaultLayout $ do
        setTitle $ toHtml $ "/" `append` bname `append` "/ | Lauta"
        $(widgetFile "board")

-- /board[b]/#[t]/
getThreadR :: Text -> BoardpostId -> Handler RepHtml
getThreadR bname boardpostId = do
    (op, rest) <- runDB $ do
        op <- get404 boardpostId
        --rest <-
        return (op, Nothing)
    defaultLayout $ do
        setTitle $ toHtml $ "/" `append` bname `append` "/ | Lauta"

postThreadR :: Text -> BoardpostId -> Handler RepHtml
postThreadR bname tid = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    ((res, formWidget), encType) <- runFormPost (postForm (entityKey board))
--  thread <- runDB $ get tid
    defaultLayout $ do
        setTitle $ toHtml $ "/" `append` bname `append` "/ | Lauta"

postForm :: BoardId -> Form Boardpost
postForm bid = renderDivs $ Boardpost
    <$> pure bid
    <*> pure Nothing
    <*> aformM (liftIO getCurrentTime)
    <*> aopt textField "Name" Nothing
    <*> aopt emailField "Email" Nothing
    <*> aopt textField "Title" Nothing
    <*> aopt textareaField "Content" Nothing
    <*> areq textField "Password" Nothing
