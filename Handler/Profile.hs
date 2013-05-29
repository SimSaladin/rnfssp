module Handler.Profile where

import Import
import qualified Data.Text as T
import Data.Maybe
import Yesod.Auth.HashDB (setPassword, validateUser)

renderProfile :: User -> Widget -> Handler RepHtml
renderProfile user w = defaultLayout $ do
    navigation "Profile"
    setTitle . toHtml . T.append "Profile of" . userUsername $ user
    w

getProfileR :: Handler RepHtml
getProfileR = do
  Entity uid uval <- requireAuth
  (passwdW, encType) <- generateFormPost $ userUpdateForm uval
  renderProfile uval $(widgetFile "profile")
  where own = True

postProfileR :: Handler RepHtml
postProfileR = do
  Entity uid uval <- requireAuth
  ((res, passwdW), encType) <- runFormPost $ userUpdateForm uval
  case res of
    FormSuccess action -> do
      user <- action
      runDB $ replace uid user
      setMessage "User info updated."
      redirect ProfileR
    _ -> renderProfile uval $(widgetFile "profile")
  where own = True

userUpdateForm :: User -> Form (Handler User)
userUpdateForm user = renderBootstrap $ f
  <$> aopt emailField "Sähköposti" (Just $ Just $ userEmail user)
  <*> aopt passwordConfirmField "" Nothing
  <*> areq (checkM checkPassword passwordField) "Tämänhetkinen salasana" Nothing
    where
  f email new_pw _ = (if isJust new_pw then setPassword (fromJust new_pw) else return)
                     user{userEmail = fromMaybe "" email}

  checkPassword    = liftM checkHelper . validateUser (UniqueUser $ userUsername user)
  checkHelper True  = Right ""
  checkHelper False = Left ("Salasana ei täsmää." :: Text)
      
