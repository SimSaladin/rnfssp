module Handler.Profile where

import Import

getProfileR :: Handler RepHtml
getProfileR = do
  Entity uid uval <- requireAuth
  defaultLayout $ do
    setTitle $ toHtml $ userUsername uval `T.append` "'s profile"
    $(widgetFile "profile")

postProfileR :: Handler RepHtml
postProfileR = error "Not yet implemented: postProfileR"
