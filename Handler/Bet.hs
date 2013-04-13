module Handler.Bet where

import Import
import Data.Time (getCurrentTime)

getBetR :: Handler RepHtml
getBetR = do
  uid <- requireAuthId
  (newBetW, encType) <- generateFormPost $ newBetForm uid
  bets <- runDB $ selectList [BettargetRunning ==. True] [Asc BettargetDate]
  defaultLayout $ do
      setTitle "Betting"
      $(widgetFile "bets")

postBetR :: Handler RepHtml
postBetR = do
  setMessage "POST not yet implemented @postBetR"
  redirect BetR

getBetViewR :: BetId -> Handler RepHtml
getBetViewR _ = error "Not yet implemented: getBetR"

postBetViewR :: BetId -> Handler RepHtml
postBetViewR _ = error "Not yet implemented: postBetR"

newBetForm :: UserId -> Form Bettarget
newBetForm owner = renderBootstrap $ Bettarget
  <$> aformM (liftIO getCurrentTime)
  <*> pure True
  <*> pure owner
  <*> areq textField "Description" Nothing
  <*> areq mapField "Options and Factors" Nothing

bets2table :: [Bettarget] -> Widget
bets2table targets = [whamlet|$newline never
<table>
  <tr>
    <th>Opened
    <th>Closes
    <th>Description
  $forall bet <- targets
    <tr>
      <td>#{show $ bettargetDate bet}
      <td>...
      <td>#{bettargetTitle bet}
  |]
