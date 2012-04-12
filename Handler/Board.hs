module Handler.Board
    ( getBoardHomeR
    , getBoardR
    , postBoardR
    , getThreadR
    , postThreadR
    , widgetThreadPreview
    , widgetThreadPost
    ) where

import Import
import Data.Text (append)
import Data.Int (Int64)
import Data.Time (getCurrentTime)
-- import Yesod.Form.Nic (nicHtmlField)

-- /board
getBoardHomeR :: Handler RepHtml
getBoardHomeR = do
    boards <- runDB $ selectList ([] :: [Filter Board]) []
    defaultLayout $ do
        setTitle "Lauta"
        $(widgetFile "board-home")

-- /board/b
getBoardR :: Text -> Handler RepHtml
getBoardR bname = do
    (board, postOps) <- runDB $ do
        b <- getBy404 $ UniqueBoard bname
        ops <- selectList
                    [ BoardpostLocation ==. entityKey b
                    , BoardpostParent ==. Nothing
                    ]
                    [Asc BoardpostTime]
        return (b, ops)
    (formWidget, encType) <- generateFormPost (postForm (entityKey board) Nothing)
    defaultLayout $ do
        setTitle $ toHtml $ "/" `append` bname `append` "/ | Lauta"
        $(widgetFile "board")

postBoardR :: Text -> Handler RepHtml
postBoardR bname = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    ((result, _), _) <- runFormPost (postForm (entityKey board) Nothing)
    case result of
        FormSuccess post -> do
            postId <- runDB $ insert post
            setMessage "Postaus onnistui!"
            redirect $ ThreadR bname postId
        _ -> do
            setMessage "Postaus failasi"
            redirect $ BoardR bname

-- /board/b/1
getThreadR :: Text -> BoardpostId -> Handler RepHtml
getThreadR bname pid = do
    (board, postOp, postReplies) <- runDB $ do
        board <- getBy404 $ UniqueBoard bname
        op <- get404 pid
        replies <- selectList [BoardpostParent ==. Just pid] [Asc BoardpostTime]
        return (board, op, replies)
    (postWidget, encType) <- generateFormPost (postForm (entityKey board) (Just pid)) --FIXME
    defaultLayout $ do
        setTitle $ toHtml $ "/" `append` bname `append` "/ | Lauta"
        $(widgetFile "board-thread")

postThreadR :: Text -> BoardpostId -> Handler RepHtml
postThreadR bname pid = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    ((result, _), _) <- runFormPost (postForm (entityKey board) Nothing) --FIXME
    case result of
        FormSuccess reply -> do
            _ <- runDB $ insert reply
            setMessage "Postaus onnistui!"
        _ -> setMessage "Hmm.. jotain meni pieleen"
    redirect $ ThreadR bname pid

widgetThreadPreview :: Key (PersistEntityBackend Boardpost) Boardpost -> BoardpostGeneric a ->  Widget
widgetThreadPreview n op = do
    widgetThreadPost n op
    -- TODO: print ~3 replies

widgetThreadPost :: Key (PersistEntityBackend Boardpost) Boardpost -> BoardpostGeneric a -> Widget
widgetThreadPost n reply = do
    let isop = case boardpostParent reply of
            Nothing -> False
            Just _ -> True
    let time = show $ boardpostTime reply
    let poster = case boardpostPoster reply of
            Nothing -> ""
            Just p -> p
    -- email in template; no <a> tags if no email
    let title = case boardpostTitle reply of
            Nothing -> ""
            Just t -> t
    let content = case boardpostContent reply of
            Nothing -> toHtml ("" :: Text)
            Just c -> toHtml c
    let number = case fromPersistValue (unKey n) :: Either Text Int64 of
            Left _ -> "fail!" -- this case :: Text, but String in other case.
            Right num -> show num
    $(widgetFile "board-post")

postForm :: BoardId -> Maybe BoardpostId -> Form Boardpost
postForm bid mpid = renderDivs $ Boardpost
    <$> pure bid
    <*> pure mpid
    <*> aformM (liftIO getCurrentTime)
    <*> aopt textField "Name" Nothing
    <*> aopt emailField "Email" Nothing
    <*> aopt textField "Title" Nothing
    <*> aopt textareaField "Content" Nothing
    <*> areq textField "Password" Nothing
