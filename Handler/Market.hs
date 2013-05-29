------------------------------------------------------------------------------
-- File:          Handler/Market.hs
-- Creation Date: May 28 2013 [17:49:23]
-- Last Modified: May 29 2013 [16:59:40]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Handler.Market where

import Import
import Utils
import Data.List (head, groupBy)

getMarketHomeR :: Handler RepHtml
getMarketHomeR = do
  denyIfAnonUnPVL
  muser <- maybeAuth
  (buyerForm, buyerEnctype) <- generateFormPost $ buyForm $ entityVal <$> muser
  (sellerForm, sellerEnctype) <- generateFormPost $ sellForm $ entityVal <$> muser
  defaultLayout $ do
    navigation "Market"
    $(widgetFile "market_home")

postMarketNewBuyItemR :: Handler RepHtml
postMarketNewBuyItemR = do
    muser <- maybeAuth
    ((res, _), _) <- runFormPost $ buyForm $ entityVal <$> muser
    handleItem (Left res)

handleItem :: Either (FormResult BuyItem) (FormResult SaleItem) -> Handler RepHtml
handleItem sitem = do
    setUltDestReferer
    case sitem of
        Right item -> dostuff' item "Kohde " saleItemName " lisätty myyntiin."
        Left  item -> dostuff item "Osto-ilmoitus " buyItemWhat " lisätty."
  where
    dostuff res r m l = case res of
      FormSuccess item -> do
          iid <- runDB $ insert item
          setMessage $ toHtml $ r <> m item <> l
          redirectUltDest MarketHomeR
      FormMissing   -> setMessage "Tiedot olivat tyhjiä!" >> redirectUltDest MarketHomeR
      FormFailure _ -> setMessage "Ilmoituksen lisääminen ei onnistunut." >> getMarketHomeR

    -- FIXME something more elegant than copypaste? :)
    dostuff' res r m l = case res of
      FormSuccess item -> do
          iid <- runDB $ insert item
          setMessage $ toHtml $ r <> m item <> l
          redirectUltDest MarketHomeR
      FormMissing   -> setMessage "Tiedot olivat tyhjiä!" >> redirectUltDest MarketHomeR
      FormFailure _ -> setMessage "Ilmoituksen lisääminen ei onnistunut." >> getMarketHomeR

postMarketNewSaleItemR :: Handler RepHtml
postMarketNewSaleItemR = do
  muser <- maybeAuth
  ((res,_),_) <- runFormPost $ sellForm $ entityVal <$> muser
  handleItem $ Right res

buyForm :: (Maybe User) -> Form BuyItem
buyForm muser = renderBootstrap $ BuyItem
  <$> areq textField  "Haluaisin ostaa:" Nothing
  <*> areq intField   "Näin monta:"      (Just 1)
  <*> ((unTextarea <$>) <$> aopt textareaField "Lisätietoja:" Nothing)
  <*> areq textField  "Olen" (userUsername <$> muser)
  <*> aopt emailField "Sähköpostiosoitteeni on" (Just . userEmail <$> muser)
  <*> aopt textField  "IRC-nickini on"          (userIrcnick <$> muser)

sellForm :: (Maybe User) -> Form SaleItem
sellForm muser = renderBootstrap $ SaleItem
  <$> areq textField  "Myyn:"       Nothing
  <*> areq intField   "Näin monta:" (Just 1)
  <*> areq doubleField "Hinta"      Nothing
  <*> ((unTextarea <$>) <$> aopt textareaField  "Lisätietoja:" Nothing)
  <*> areq textField  "Olen"                    (userUsername <$> muser)
  <*> aopt emailField "Sähköpostiosoitteeni on" (Just . userEmail <$> muser)
  <*> aopt textField  "IRC-nickini on"          (userIrcnick <$> muser)

buy, sale :: Widget
buy = do
  xs <- liftM (groupBy $ \x y -> let f = buyItemBuyer . entityVal in f x == f x) $
        liftHandlerT $ runDB $ selectList [] [Asc BuyItemBuyer, Asc BuyItemWhat]
  [whamlet|
<table>
  <thead>
    <tr>
      <th>Kuka
      <th>Mitä
      <th>Hinta
      <th>
  <tbody>
    $forall ys <- xs
      $forall Entity k x <- ys
        <tr>
          $with person <- buyItemBuyer $ entityVal $ head ys
            <th>#{person}
          <td>#{buyItemWhat x} #
            $if (/=) (buyItemCount x) 1
              (#{buyItemCount x} kpl) #
            $maybe desc <- buyItemDesc x
              <span .market-description>- #{desc}
  |]

sale = do
  xs <- liftHandlerT $ runDB $ selectList [] [Asc SaleItemName]
  [whamlet|
<ul>
  $forall Entity k x <- xs
    <li>
      <a>#{saleItemName x}
  |]
