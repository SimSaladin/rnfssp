module Handler.Admin 
    ( getAdminR
    , postAdminR
    ) where

import Import
import Utils
import qualified Handler.Media as Media (adminWidget)

getAdminR :: Handler Html
getAdminR = do
    at <- (liftM2 (,) `on` lookupGetParam) "action" "target"
    case at of
        (Just "noapprove_user", Just u) -> setApprove u False >> redirect AdminR
        (Just "approve_user"  , Just u) -> setApprove u True  >> redirect AdminR
        (Just "update_media"  ,      _) -> giveUrlRenderer mempty
        (Just _,_)                      -> invalidArgs ["Unknown action!"]
        (     _,_)                      -> do
            (formWidget, encType) <- generateFormPost newboardForm
            let form = renderFormH $ myForm
                  MsgBoardCreate encType AdminR
                  formWidget (submitButtonI MsgBoardCreate) FormMissing

            users <- runDB $ selectList [] [Asc UserUsername]
            defaultLayout $ do
                navigation "Admin"
                setTitle "Adminstration"
                $(widgetFile "admin")
  where
    (disapprove, approve) = ("disapprove_user", "approve_user") :: (Text, Text)
    usert = ((,,) <$> userUsername
                  <*> userComment
                  <*> (\v -> if' (userValid v) disapprove approve)
            ) . entityVal

    setApprove name value = runDB $
        updateWhere [UserUsername ==. name] [UserValid =. value]

postAdminR :: Handler Html
postAdminR = do
    ((result, _), _) <- runFormPost newboardForm
    case result of
        FormSuccess board -> do
            void . runDB $ insert board
            setMessage $ toHtml $ "New board added: " <> boardName board
        _ -> setMessage "Something went wrong while creating the board"
    redirect AdminR

newboardForm :: Form Board
newboardForm = renderBootstrap $ Board
    <$> areq textField "Name" Nothing
    <*> areq textField "Description" Nothing
