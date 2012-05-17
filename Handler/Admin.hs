module Handler.Admin 
    ( getAdminR
    , postAdminR
    ) where

import Import
import qualified Data.Text as T (append)
import Handler.Media (pathAnime)
import System.Directory (getDirectoryContents, doesDirectoryExist)
import System.FilePath (combine)

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
    act <- lookupGetParam "action"
    case act of
        Just a
            | a == "update_media" -> do
                -- do smth for this staircase?
                files <- liftIO $ rDirContents pathAnime
                _ <- runDB $ do 
                    found <- mapM (\fp -> do
                        entry <- getBy $ UniqueAnimefile fp
                        return entry
                        ) files
                    return found

                    --dbFiles <- selectList ([] :: [Filter AnimeFile]) []
                hamletToRepHtml [hamlet||]
            | otherwise -> do
                setMessage "Unknown action!"
                redirect $ AdminR
        Nothing -> do
            (boardAddWidget, encType) <- generateFormPost newboardForm
            let submission = Nothing :: Maybe Board
            defaultLayout $ do
                setTitle "admin"
                $(widgetFile "admin")

postAdminR :: Handler RepHtml
postAdminR = do
    ((result, boardAddWidget), encType) <- runFormPost newboardForm
    boards <- runDB $ selectList ([] :: [Filter Board]) []
    case result of
        FormSuccess board -> do
            _ <- runDB $ insert board
            setMessage $ toHtml $ "Uusi lauta luotu: " `T.append` (boardName board)
        _ -> do
            setMessage "Hmmm.. jokin meni pieleen lautaa luotaessa"
    defaultLayout $ do
        setTitle "Lauta"
        $(widgetFile "admin")

newboardForm :: Form Board
newboardForm = renderDivs $ Board
    <$> areq textField "Nimi" Nothing
    <*> areq textField "Kuvaus" Nothing

