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
import Prelude (tail, head)
import System.FilePath
import qualified Data.Text as T
import Data.Int (Int64)
import Data.Time (getCurrentTime, UTCTime)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (toModifiedJulianDay)
import Data.Maybe (fromMaybe, isNothing)

import Utils

type Thread = (Entity Boardpost, [Entity Boardpost])

-- [/board] 
getBoardHomeR :: Handler Html
getBoardHomeR = do
    boards <- runDB $ selectList [] [Asc BoardName]
    defaultLayout $ do
        setTitle "Lauta"
        navigation "Lauta"
        $(widgetFile "board-home")

renderThreads :: Entity Board
              -> Either Thread [Thread]
              -> ((FormResult (Maybe String -> Maybe Text -> Boardpost, Maybe FileInfo), Widget), Enctype) -- 
              -> Handler Html
renderThreads (Entity _ bval) content ((result, formWidget), encType) = defaultLayout $ do
    setTitle . toHtml $ "/" <> boardName bval <> "/"
    navigation "Lauta"
    $(widgetFile "board")

-- /board/<board>
getBoardR :: Text -> Handler Html
getBoardR bname = do
    (board, threads) <- runDB $ do
        b   <- getBy404 $ UniqueBoard bname
        ops <- selectList [ BoardpostLocation ==. entityKey b
                          , BoardpostParent   ==. Nothing ]
                          [Asc BoardpostTime]
        replies <- mapM (\op -> selectList [BoardpostParent ==. Just (entityKey op)] []) ops
        return (b, zip ops replies)
    form <- runFormPost $ postForm (entityKey board) Nothing
    renderThreads board (Right threads) form

-- /board/<board> - starting new thread.
postBoardR :: Text -> Handler Html
postBoardR bname = do
    Entity bid _ <- runDB $ getBy404 $ UniqueBoard bname
    ((result, _), _) <- runFormPost $ postForm bid Nothing
    case result of
      FormSuccess post -> handlePost post >>= runDB . insert >>= redirect . ThreadR bname
      FormFailure _ -> getBoardR bname
      FormMissing   -> setMessage "Tyhjä!" >> redirect (BoardR bname)

-- /board/b/1
getThreadR :: Text -> BoardpostId -> Handler Html
getThreadR bname opKey = do
    (board, thread) <- runDB $ do
        board   <- getBy404 $ UniqueBoard bname
        op      <- get404 opKey
        replies <- selectList [BoardpostParent ==. Just opKey] [Asc BoardpostTime]
        return (board, (Entity opKey op, replies))
    form <- runFormPost (postForm (entityKey board) (Just opKey))
    renderThreads board (Left thread) form

postThreadR :: Text -> BoardpostId -> Handler Html
postThreadR bname opKey = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    ((result, _), _) <- runFormPost (postForm (entityKey board) (Just opKey))
    case result of
        FormSuccess post   -> handlePost post >>= void . runDB . insert
        -- TODO: failures in the form instead
        FormFailure fails  -> setMessage $ toHtml $ T.intercalate "\n" fails
        FormMissing        -> setMessage "no POST data was received."
    redirect $ ThreadR bname opKey

getBoardFileR :: String -> Handler Html
getBoardFileR fname = getFilesPath >>= \p -> sendFile "" $ p </> fname

widgetThreadPost :: Text -> Key Boardpost -> Boardpost -> Widget
widgetThreadPost bname n reply = do
    let isop    = isNothing $ boardpostParent reply
    let time    = show $ boardpostTime reply
    let poster  = fromMaybe "anonyymi" (boardpostPoster reply)
    let title   = fromMaybe "" (boardpostTitle reply)
    let number  = key2text n
    let divClass = if isop then "postop" else "postreply" :: String
    $(widgetFile "board-post")

getFilesPath :: Handler FilePath
getFilesPath = fmap ((</> "board") . extraDirDyn) getExtra

key2text :: Key v -> String
key2text n = case fromPersistValue $ unKey n :: Either Text Int64 of
                Left _    -> error "key2text: no parse"
                Right num -> show num

handlePost :: (Maybe String -> Maybe Text -> Boardpost, Maybe FileInfo) -> Handler Boardpost
handlePost (topost, Nothing)   = return (topost Nothing Nothing)
handlePost (topost, Just info)
    | T.null fname             = return (topost Nothing Nothing)
    | otherwise                = do
        dir <- getFilesPath
        liftM (flip topost (Just fname) . Just . takeFileName) . liftIO $ do
            time <- liftM (show . toModifiedJulianDay . utctDay) getCurrentTime
            here <- uniqueFilePath dir (time ++ '_' : T.unpack fname)
            fileMove info here
            return here
    where fname = fileName info

postForm :: BoardId -> Maybe BoardpostId -> Form (Maybe String -> Maybe Text -> Boardpost, Maybe FileInfo)
postForm bid mpid = renderBootstrap $ 
    (\x1 x2 x3 x4 x5 x6 x7 x9 x8 -> (Boardpost x1 x2 x3 x4 x5 x6 x7 x8, x9) )
    <$> pure bid
    <*> pure mpid
    <*> lift (liftIO getCurrentTime)
    <*> aopt textField "Nimimerkki" Nothing
    <*> aopt emailField "Sähköposti" Nothing
    <*> aopt textField "Aihe" Nothing
    <*> aopt textareaField "Viesti" Nothing
    <*> fileAFormOpt "Liite"
    <*> areq passwordField "Salasana" (Just "salasana")
