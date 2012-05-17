{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import qualified Data.Text as T (length)
import System.IO.Unsafe (unsafePerformIO)
import Yesod.Auth.HashDB (setPassword)

getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

getRegisterR :: Handler RepHtml
getRegisterR = do
    (formWidget, encType) <- generateFormPost registerForm
    defaultLayout $ do
        setTitle "Register"
        $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
    ((creds, formWidget), encType) <- runFormPost registerForm
    case creds of
        FormSuccess (username, password) -> do
            uid <- runDB $ getBy $ UniqueUser username
            case uid of
                Just _ -> do
                    setMessage "Username .. on jo käytössä"
                    defaultLayout $ do
                        $(widgetFile "register")
                Nothing -> do
                    -- fixme: other options exist besides unsafePerformIO?
                    uid <- runDB $ insert $ unsafePerformIO $ setPassword password (User username "" "" False)
                    setMessage "Rekisteröinti onnistui"
                    redirect $ HomeR -- todo: profile page for uid
        _ -> do
            setMessage "Annetetut tiedot eivät käy"
            defaultLayout $ do
                $(widgetFile "register")

registerForm :: Form (Text, Text)
registerForm = renderDivs $ (,)
    <$> areq textField "Username" Nothing
    <*> areq passwordConfirmField "Password" Nothing

passwordConfirmField :: Field sub master Text
passwordConfirmField = Field
    { fieldParse = \rawVals ->
        case rawVals of
            [a, b]
                | T.length a < 4 -> return $ Left "Password must be at least 4 characters"
                | a == b -> return $ Right $ Just a
                | otherwise -> return $ Left "Passwords don't match"
            _ -> return $ Left "You must enter two values"
    , fieldView = \idAttr nameAttr _ eResult isReq -> [whamlet|
<input id=#{idAttr} name=#{nameAttr} type=password :isReq:required>
<div .required>
    <label>Confirm:
    <input id=#{idAttr}-confirm name=#{nameAttr} type=password :isReq:required>
|]
    }
