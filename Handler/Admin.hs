module Handler.Admin 
    ( getAdminR
    , postAdminR
    ) where

import Import
import qualified Data.Text as T (append)
import qualified Handler.Media as Media (adminWidget)

getAdminR :: Handler RepHtml
getAdminR = do
    action <- lookupGetParam "action"
    target <- lookupGetParam "target"
    case (action, target) of
        (Just "noapprove_user", Just u) -> setApprove u False >> redirect AdminR
        (Just "approve_user"  , Just u) -> setApprove u True  >> redirect AdminR
        (Just "update_media"  ,      _) -> hamletToRepHtml [hamlet||]
        (Just _,_) -> invalidArgs ["Unknown action!"]
        (     _,_) -> do
            (formWidget, encType) <- generateFormPost newboardForm
            users <- liftM (map usert) $ runDB $ selectList ([] :: [Filter User]) []
            defaultLayout $ do
                navigation "Admin"
                setTitle "Adminstration"
                $(widgetFile "admin")
  where
    usert ent = (userUsername val, userComment val, act) where
        val = entityVal ent
        act = if userValid val then "disapprove_user" else "approve_user" :: Text

    setApprove name value = runDB $ do
        Entity key _ <- getBy404 $ UniqueUser name
        update key [UserValid =. value]

postAdminR :: Handler RepHtml
postAdminR = do
    ((result, _), _) <- runFormPost newboardForm
    case result of
        FormSuccess board -> do
            _ <- runDB $ insert board
            setMessage $ toHtml $ "New board added: " `T.append` boardName board
        _ -> setMessage "Something went wrong while creating the board"
    redirect AdminR

newboardForm :: Form Board
newboardForm = renderDivs $ Board
    <$> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
