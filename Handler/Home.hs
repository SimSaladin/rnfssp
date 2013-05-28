{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import           Data.Maybe (isJust)
import           Yesod.Auth.HashDB (setPassword)
import Import hiding (fail)
import Chat
import Handler.Blog

getHomePageR :: Handler RepHtml
getHomePageR = defaultLayout $ do
    setTitle "SS"
    navigation "SS"
    $(widgetFile "home")

renderRegister :: Widget -> Handler RepHtml
renderRegister w = defaultLayout $
    setTitle "Registration" >> navigation "Register" >> w

getRegisterR :: Handler RepHtml
getRegisterR = do
    homeIfLoggedIn
    (widget, encType) <- generateFormPost registerForm
    renderRegister $(widgetFile "register")

postRegisterR :: Handler RepHtml
postRegisterR = do
    homeIfLoggedIn
    ((res, widget), encType) <- runFormPost registerForm
    case res of
        FormSuccess user -> do
            void $ runDB . insert =<< setPassword (userPassword user) user
            setMessage "Your registration has been received and is waiting for approval."
            -- TODO: send email?
            redirect HomePageR
        _ -> renderRegister $(widgetFile "register")

registerForm :: Form User
registerForm = renderBootstrap $
    ( \name email pass comment ->
        User name pass email "" False False Nothing comment)
    <$> areq (checkM uniqueUsername textField) "Username" Nothing
    <*> areq (checkM uniqueEmail emailField) "Email" Nothing
    <*> areq passwordConfirmField "" Nothing
    <*> areq textareaField "Comment" Nothing
  where
    uniqueUsername name = do
        muid <- runDB $ getBy $ UniqueUser name
        return $ if isJust muid
           then Left $ "The username \"" `mappend` name `mappend` "\"is already in use!"
           else Right name

    uniqueEmail email = do
        muid <- runDB $ selectList [ UserEmail ==. email ] []
        return $ case muid of
            [] -> Right email
            _  -> Left $ ("There is already an account registered with this email." :: Text)
