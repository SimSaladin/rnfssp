{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import           Yesod.Auth.HashDB (setPassword)
import Import hiding (fail)
import Chat
import Handler.Blog
import Handler.Media

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
    ( \name email irc pass comment ->
        User name email irc pass "" False False Nothing comment)
    <$> areq (checkM uniqueUsername textField) "Username" Nothing
    <*> areq (checkM uniqueEmail emailField) "Email" Nothing
    <*> aopt (checkM uniqueIrcnick textField) "Irc-nick" Nothing
    <*> areq passwordConfirmField "" Nothing
    <*> areq textareaField "Comment" Nothing
  where
    uniqueUsername name  = f name ("The username \"" <> name <> "\"is already in use!")      $ getBy $ UniqueUser name
    uniqueEmail    email = f email "There is already an account registered with this email." $ selectFirst [UserEmail ==. email] []
    uniqueIrcnick  nick  = f nick "Tämä nick on jo käytössä."                                $ selectFirst [UserIrcnick ==. Just nick] []

    f a msg = liftM (maybe (Right a) (const $ Left (msg::Text))) . runDB

