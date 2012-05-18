module Handler.Admin 
    ( getAdminR
    , postAdminR
    ) where

import Import
import qualified Data.Text as T (append)
--import Handler.Media (pathAnime)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath (combine)

-- | list (sub)dirs and files
rDirContents :: FilePath -> IO [FilePath]
rDirContents fp = do
    is_dir <- doesDirectoryExist fp
    dirc <- case is_dir of
        True -> getDirectoryContents fp
        False -> pure []
    contents <- mapM (\x -> rDirContents $ combine fp x) $ drop 2 dirc
    return $ concat contents ++ [fp]

getAdminR :: Handler RepHtml
getAdminR = do
    users <- runDB $ selectList ([] :: [Filter User]) [] >>= \query ->  return $ map (\x ->
        ( userUsername $ entityVal x
        , if userValid $ entityVal x then "disapprove_user" else "approve_user" :: Text))
        query
    appr <- lookupGetParam "approve_user"
    --disappr <- lookupGetParam "disapprove_user"
    case appr of
        Just a -> runDB $ do
            uid <- getBy $ UniqueUser a
            case uid of
                Just (Entity k _) -> update k [UserValid =. True]
                Nothing -> return ()
        Nothing -> return ()
    lookupGetParam "action" >>= \act -> case act of
        Just a
            | a == "update_media" -> do
                hamletToRepHtml [hamlet||]
            | otherwise -> do
                invalidArgs ["Unknown action!"]
        Nothing -> do
            (formWidget, encType) <- generateFormPost newboardForm
            let submission = Nothing :: Maybe Board
            defaultLayout $ do
                setTitle "admin"
                $(widgetFile "admin")

postAdminR :: Handler RepHtml
postAdminR = do
    ((result, _), _) <- runFormPost newboardForm
    boards <- runDB $ selectList ([] :: [Filter Board]) []
    case result of
        FormSuccess board -> do
            _ <- runDB $ insert board
            setMessage $ toHtml $ "New board added: " `T.append` (boardName board)
        _ -> do
            setMessage "Something went wrong while creating the board"
    redirect $ AdminR

newboardForm :: Form Board
newboardForm = renderDivs $ Board
    <$> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
