module Handler.Board
    ( getBoardHomeR
    , getBoardR
    , postBoardR
    , getThreadR
    , postThreadR
    , getBoardFileR
    , widgetThreadPost
    ) where

import Import
import Prelude (tail)
import System.IO (openBinaryTempFile, hClose)
import System.FilePath (takeFileName, takeExtension, combine)
import Data.Text (append, unpack)
import Data.Int (Int64)
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (toModifiedJulianDay)
import Data.Maybe (fromMaybe, isNothing)
import Data.ByteString.Lazy (hPut)
-- import Yesod.Form.Nic (nicHtmlField)

imgFilepath :: FilePath
imgFilepath = "/home/sim/rnfssp/files/board/"

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
    (board, ops, replies) <- runDB $ do
        b <- getBy404 $ UniqueBoard bname
        ops <- selectList
                    [ BoardpostLocation ==. entityKey b
                    , BoardpostParent ==. Nothing
                    ]
                    [Asc BoardpostTime]
        replies <- mapM (\op -> selectList [BoardpostParent ==. Just (entityKey op)] []) ops
        return (b, ops, replies)
    let previews = zip ops replies
    (formWidget, encType) <- generateFormPost (postForm (entityKey board) Nothing)
    defaultLayout $ do
        setTitle $ toHtml $ "/" `append` bname `append` "/ :: Lauta"
        $(widgetFile "board")

postBoardR :: Text -> Handler RepHtml
postBoardR bname = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    ((result, _), _) <- runFormPost (postForm (entityKey board) Nothing)
    case result of
        FormSuccess replyD -> do
            save <- liftIO $ savePost replyD
            case save of
                Just p -> do
                    postId <- runDB $ insert p
                    setMessage "Postaus onnistui!"
                    redirect $ ThreadR bname postId
                Nothing -> do
                    setMessage "Postaus failasi"
                    redirect $ BoardR bname
        _ -> do
            setMessage "Postaus failasi"
            redirect $ BoardR bname

-- /board/b/1
getThreadR :: Text -> BoardpostId -> Handler RepHtml
getThreadR bname opKey = do
    (board, opVal, replies) <- runDB $ do
        board <- getBy404 $ UniqueBoard bname
        op <- get404 opKey
        replies <- selectList [BoardpostParent ==. Just opKey] [Asc BoardpostTime]
        return (board, op, replies)
    (postWidget, encType) <- generateFormPost (postForm (entityKey board) (Just opKey))
    defaultLayout $ do
        setTitle $ toHtml $ "/" `append` bname `append` "/ :: Lauta"
        $(widgetFile "board-thread")

postThreadR :: Text -> BoardpostId -> Handler RepHtml
postThreadR bname opKey = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    ((result, _), _) <- runFormPost (postForm (entityKey board) (Just opKey))
    case result of
        FormSuccess replyD -> do
            save <- liftIO $ savePost replyD
            case save of
                Just p -> do
                    _ <- runDB $ insert p
                    setMessage "Postaus onnistui!"
                Nothing -> do
                    setMessage "Postaus failasi"
        FormFailure fails -> do
            setMessage $ toHtml $ toHtml <$> append "\n" <$> fails
        FormMissing -> do
            setMessage "no POST data"
    redirect $ ThreadR bname opKey

getBoardFileR :: String -> Handler RepHtml
getBoardFileR fname = do
    sendFile "" $ combine imgFilepath fname

widgetThreadPost :: Text -> Key (PersistEntityBackend Boardpost) Boardpost -> BoardpostGeneric a -> Widget
widgetThreadPost bname n reply = do
    let isop = isNothing $ boardpostParent reply
    let time = show $ boardpostTime reply
    let poster = fromMaybe "anonyymi" (boardpostPoster reply)
    let title = fromMaybe "" (boardpostTitle reply)
    let content = case boardpostContent reply of
            Nothing -> toHtml ("" :: Text)
            Just c -> toHtml c
    let number = key2text n
    let divClass = if isop then "postop" else "postreply" :: String
    $(widgetFile "board-post")

key2text :: Key (PersistEntityBackend Boardpost) Boardpost -> String
key2text n = case fromPersistValue $ unKey n :: Either Text Int64 of
                Left _ -> "fail!"
                Right num -> show num

data D1 = D1
    { d1location :: BoardId
    , d1parent :: Maybe BoardpostId
    , d1time :: UTCTime
    , d1fileinfo :: Maybe FileInfo
    , d1poster :: Maybe Text
    , d1email :: Maybe Text
    , d1title :: Maybe Text
    , d1content :: Maybe Textarea
    , d1pass :: Text
    }

savePost :: D1 -> IO (Maybe Boardpost)
savePost d = do
    (fname, info) <- case d1fileinfo d of
        Nothing -> return (Nothing, Nothing) :: IO (Maybe FilePath, Maybe Text)
        Just fi -> do
            t <- liftIO getCurrentTime
            let time = show $ toModifiedJulianDay $ utctDay t
                ext  = takeExtension $ unpack $ fileName fi
            (fp, h) <- openBinaryTempFile imgFilepath (time ++ ext)
            hPut h $ fileContent fi
            hClose h
            return (Just $ takeFileName fp, Just $ fileName fi)

    return $ Just $ Boardpost
            (d1location d) (d1parent d) (d1time d)
            (d1poster d) (d1email d) (d1title d)
            (d1content d) (d1pass d) fname info

postForm :: BoardId -> Maybe BoardpostId -> Form D1
postForm bid mpid = renderDivs $ D1
    <$> pure bid
    <*> pure mpid
    <*> aformM (liftIO getCurrentTime)
    <*> fileAFormOpt "Liite"
    <*> aopt textField "Nimimerkki" Nothing
    <*> aopt emailField "Sähköposti" Nothing
    <*> aopt textField "Aihe" Nothing
    <*> aopt textareaField "Viesti" Nothing
    <*> areq passwordField "Salasana" (Just "salasana")
