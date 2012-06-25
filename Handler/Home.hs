{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import qualified Data.Text as T
import Data.Maybe (isJust)
import Yesod.Auth.HashDB (setPassword)

getRegisterR :: Handler RepHtml
getRegisterR = do
    (widget, encType) <- generateFormPost registerForm
    let fails = [] :: [Text]
        in defaultLayout $ do setTitle "Register"
                              $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
    ((res, widget), encType) <- runFormPost registerForm
    let reperr fails = defaultLayout $ do setTitle "Register ERROR"
                                          $(widgetFile "register")
    case res of
        FormSuccess user -> let name = userUsername user in do
            muid <- runDB $ getBy $ UniqueUser name
            if isJust muid
               then reperr [T.intercalate " " ["Username", name, "on jo käytössä"]]
               else do newUser <- setPassword (userPassword user) user
                       uid <- runDB $ insert newUser
                       setMessage "Rekisteröinti onnistui"
                       redirect $ AuthR LoginR
        FormFailure fails -> reperr fails
        FormMissing -> redirect $ RegisterR

registerForm :: Html -> MForm App App (FormResult User, Widget)
registerForm extra = do
    (resName, viewName) <- mreq textField "Username" Nothing
    (resComm, viewComm) <- mreq textareaField "Comment" Nothing
    (resPass, viewPass) <- mreq passwordAndConfirmField "Password" Nothing
    let resUser = User <$> resName
                       <*> pure ""
                       <*> pure ""
                       <*> pure False
                       <*> pure False
                       <*> pure Nothing
                       <*> resComm
        in return (resUser, $(widgetFile "form-register"))
    
passwordAndConfirmField :: Field sub master Text
passwordAndConfirmField = Field
    { fieldParse = \raw -> case raw of
        [a, b]
            | T.length a < 4 -> return $ Left "Password must be at least 4 characters"
            | a == b -> return $ Right $ Just a
            | otherwise -> return $ Left "Passwords don't match"
        _ -> return $ Left "You must enter two values"
    , fieldView = \aId aName _ eRes isReq -> [whamlet|
<div.control-group>
  <div.control-label for=#{aId}> Password
  <div.controls>
    <input id=#{aId} name=#{aName} type=password :isReq:required>
<div.control-group>
  <div.control-label for=#{aId}-confirm> Confirm password
  <div.controls>
    <input id=#{aId}-confirm name=#{aName}-confirm type=password :isReq:required>
    |]
    }
