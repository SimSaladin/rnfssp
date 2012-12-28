{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import hiding (fail)
import qualified Data.Text as T
import Data.Maybe (isJust)
import Yesod.Auth.HashDB (setPassword)

getRegisterR :: Handler RepHtml
getRegisterR = do
    (widget, encType) <- generateFormPost registerForm
    let fails = [] :: [Text]
        in defaultLayout $ do setTitle "Registration"
                              $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
    ((res, widget), encType) <- runFormPost registerForm
    let reperr fails = defaultLayout $ do setTitle "Registration: ERROR"
                                          $(widgetFile "register")
    case res of
        FormSuccess user -> let name = userUsername user in do
            muid <- runDB $ getBy $ UniqueUser name
            if isJust muid
               then reperr [T.intercalate " " ["Username", name, "on jo käytössä"]]
               else do newUser <- setPassword (userPassword user) user
                       _ <- runDB $ insert newUser
                       setMessage "Your registration has been noted. Please, stand by."
                       redirect BlogOverviewR
                       -- TODO: send email?
        FormFailure fails -> reperr fails
        FormMissing -> redirect RegisterR

registerForm :: Html -> MForm App App (FormResult User, Widget)
registerForm extra = do
    (resName, viewName) <- mreq textField "Username" Nothing
    (resComm, viewComm) <- mreq textareaField "Comment" Nothing
    (resPass, viewPass) <- mreq passwordConfirmField "Password" Nothing
    let resUser = User <$> resName
                       <*> resPass
                       <*> pure ""
                       <*> pure False
                       <*> pure False
                       <*> pure Nothing
                       <*> resComm
        in return (resUser, $(widgetFile "form-register"))
