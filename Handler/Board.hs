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

-- [/board] 
getBoardHomeR :: Handler RepHtml
getBoardHomeR = do
    boards <- runDB $ selectList ([] :: [Filter Board]) []
    defaultLayout $ do
        setTitle "Lauta"
        navigation "Lauta"
        $(widgetFile "board-home")

type Thread = (Entity Boardpost, [Entity Boardpost])

renderThreads :: Entity Board
              -> Either Thread [Thread]
              -> ((FormResult D1, Widget), Enctype) -- 
              -> Handler RepHtml
renderThreads (Entity _ bval) content ((result, formWidget), encType) = defaultLayout $ do
    setTitle $ toHtml $ T.concat ["/", boardName bval, "/"]
    navigation "Lauta"
    $(widgetFile "board")

-- /board/<board>
getBoardR :: Text -> Handler RepHtml
getBoardR bname = do
    (board, threads) <- runDB $ do
        b   <- getBy404 $ UniqueBoard bname
        ops <- selectList [ BoardpostLocation ==. entityKey b
                          , BoardpostParent   ==. Nothing
                          ]
                          [Asc BoardpostTime]
        replies <- mapM (\op -> selectList [BoardpostParent ==. Just (entityKey op)] []) ops
        return (b, zip ops replies)
    form <- runFormPost (postForm (entityKey board) Nothing)
    renderThreads board (Right threads) form

-- /board/<board> - starting new thread.
postBoardR :: Text -> Handler RepHtml
postBoardR bname = do
    Entity bid _ <- runDB $ getBy404 $ UniqueBoard bname
    ((result, _), _) <- runFormPost $ postForm bid Nothing
    case result of
      FormSuccess replyD -> d1toBoardPost replyD >>= runDB . insert
                                                 >>= redirect . ThreadR bname
      FormFailure _ -> getBoardR bname -- redirect (BoardR bname)
      FormMissing -> notFound -- setMessage "Postaus failasi"

-- /board/b/1
getThreadR :: Text -> BoardpostId -> Handler RepHtml
getThreadR bname opKey = do
    (board, thread) <- runDB $ do
        board <- getBy404 $ UniqueBoard bname
        op <- get404 opKey
        replies <- selectList [BoardpostParent ==. Just opKey] [Asc BoardpostTime]
        return (board, (Entity opKey op, replies))
    form <- runFormPost (postForm (entityKey board) (Just opKey))
    renderThreads board (Left thread) form

postThreadR :: Text -> BoardpostId -> Handler RepHtml
postThreadR bname opKey = do
    board <- runDB $ getBy404 $ UniqueBoard bname
    ((result, _), _) <- runFormPost (postForm (entityKey board) (Just opKey))
    case result of
        FormSuccess replyD -> d1toBoardPost replyD >>= runDB . insert >> return ()
        -- TODO: failures in the form instead
        FormFailure fails  -> setMessage $ toHtml $ toHtml <$> T.append "\n" <$> fails
        FormMissing        -> setMessage "no POST data was received."
    redirect $ ThreadR bname opKey

getBoardFileR :: String -> Handler RepHtml
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

key2text :: Key Boardpost -> String
key2text n = case fromPersistValue $ unKey n :: Either Text Int64 of
                Left _ -> "fail!"
                Right num -> show num


-- TODO: write a single monadic form for the following stuff

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

d1toBoardPost :: D1 -> Handler Boardpost
d1toBoardPost d = do
    (fname, info) <- case d1fileinfo d of
      Nothing -> return (Nothing, Nothing)
      Just fi | T.null $ fileName fi -> return (Nothing, Nothing)
              | otherwise -> do
          dir <- getFilesPath
          to <- liftIO $ do
              t <- getCurrentTime
              to <- uniqueFilePath dir $ (++'_':T.unpack fname) $ showTime t
              putStrLn $ "--to-------------_>     " ++ to
              putStrLn $ " <--- dir -->           " ++ dir
              putStrLn $ " ---time--_>            " ++ showTime t
              putStrLn $ "--------->              " ++ T.unpack fname
              fileMove fi to >> return to
          return (Just $ takeFileName to, Just fname)
          where fname = fileName fi

    return $ Boardpost
            (d1location d) (d1parent d) (d1time d)
            (d1poster d) (d1email d) (d1title d)
            (d1content d) (d1pass d) fname info
  where
    showTime = show . toModifiedJulianDay . utctDay

postForm :: BoardId -> Maybe BoardpostId -> Form D1
postForm bid mpid = renderBootstrap $ D1
    <$> pure bid
    <*> pure mpid
    <*> aformM (liftIO getCurrentTime)
    <*> fileAFormOpt "Liite"
    <*> aopt textField "Nimimerkki" Nothing
    <*> aopt emailField "Sähköposti" Nothing
    <*> aopt textField "Aihe" Nothing
    <*> aopt textareaField "Viesti" Nothing
    <*> areq passwordField "Salasana" (Just "salasana")

