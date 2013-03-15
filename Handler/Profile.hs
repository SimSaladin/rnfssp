module Handler.Profile where

import Import
import qualified Data.Text as T
import Yesod.Auth.HashDB (setPassword, validateUser)

setTitle' :: User -> Widget
setTitle' = setTitle . toHtml . T.append "Profile of" . userUsername

getProfileR :: Handler RepHtml
getProfileR = do
  Entity uid uval <- requireAuth
  (passwdW, encType) <- generateFormPost passwordChangeForm
  defaultLayout $ do
    setTitle' uval
    navigation "Profile"
    $(widgetFile "profile")
  where own = True

postProfileR :: Handler RepHtml
postProfileR = do
  Entity uid uval <- requireAuth
  ((res, passwdW), encType) <- runFormPost passwordChangeForm
  case res of
    FormSuccess (pw, newPW) -> do
      authorized <- validateUser (UniqueUser $ userUsername uval) pw
      setMessage =<< if authorized
        then setPassword newPW uval >>= runDB . replace uid
             >> return "Password changed"
        else return "Old password didn't match"
      redirect ProfileR
    _ -> defaultLayout $ do
      setTitle' uval
      navigation "Profile"
      $(widgetFile "profile")
  where own = True

passwordChangeForm :: Form (Text, Text)
passwordChangeForm = renderBootstrap $ (,)
  <$> areq passwordField "Current password" Nothing
  <*> areq passwordConfirmField "Uusi salasana" Nothing
