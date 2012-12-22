module Handler.Admin 
    ( getAdminR
    , postAdminR
    ) where

import Import
import qualified Data.Text as T (append)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath (combine)

import qualified Handler.Media as Media (adminWidget)

getAdminR :: Handler RepHtml
getAdminR = do
    action <- lookupGetParam "action"
    target <- lookupGetParam "target"
    case (action, target) of
        (Just "noapprove_user", Just u) -> setApprove u False >> redirect AdminR
        (Just "approve_user"  , Just u) -> setApprove u True >> redirect AdminR
        (Just "update_media", _) -> hamletToRepHtml [hamlet||]
        (Just _, _) -> invalidArgs ["Unknown action!"]
        (_, _) -> do
            (formWidget, encType) <- generateFormPost newboardForm
            userEnts <- runDB $ selectList ([] :: [Filter User]) []
            let users = map usert userEnts
            defaultLayout $ do
                setTitle "Admin"
                $(widgetFile "admin")
    where
        usert ent = (userUsername val, userComment val, act) where
            val = entityVal ent
            act = if userValid val
                    then "disapprove_user" else "approve_user" :: Text

        setApprove name v = runDB $ do
            ent <- getBy $ UniqueUser name
            case ent of 
               Just x -> update (entityKey x) [UserValid =. v]
               Nothing -> return ()

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

-- | list (sub)dirs and files
rDirContents :: FilePath -> IO [FilePath]
rDirContents fp = do
    is_dir <- doesDirectoryExist fp
    dirc <- case is_dir of
        True -> getDirectoryContents fp
        False -> pure []
    contents <- mapM (\x -> rDirContents $ combine fp x) $ drop 2 dirc
    return $ concat contents ++ [fp]
