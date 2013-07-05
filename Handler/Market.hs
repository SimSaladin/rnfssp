------------------------------------------------------------------------------
-- File:          Handler/Market.hs
-- Creation Date: May 28 2013 [17:49:23]
-- Last Modified: Jul 05 2013 [22:08:58]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Handler.Market where

import Import
import Data.List (head, groupBy, union)

getMarketHomeR :: Handler RepHtml
getMarketHomeR = do
  -- XXX: not good
  -- denyIfAnonUnPVL
  muser <- maybeAuth
  (buyerForm, buyerEnctype) <- generateFormPost $ buyForm $ entityVal <$> muser
  (sellerForm, sellerEnctype) <- generateFormPost $ sellForm $ entityVal <$> muser
  items <- runDB $ do
    items'  <- liftM (map $ buyItemWhat . entityVal) $ selectList [] ([] :: [SelectOpt BuyItem])
    items'' <- liftM (map $ saleItemName . entityVal) $ selectList [] ([] :: [SelectOpt SaleItem])
    return $ items' `union` items''
  defaultLayout $ do
    navigation "Market"
    setTitle "Kauppa"
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
      FormMissing    -> setMessage "Tiedot olivat tyhjiä!" >> redirectUltDest MarketHomeR
      FormFailure xs -> setMessage (toHtml $ "Ilmoituksen lisääminen ei onnistunut: " <> foldl (<>) "" xs) >> getMarketHomeR

postMarketNewSaleItemR :: Handler RepHtml
postMarketNewSaleItemR = do
  muser <- maybeAuth
  ((res,_),_) <- runFormPost $ sellForm $ entityVal <$> muser
  handleItem $ Right res

postMarketDeleteBuyR :: BuyItemId -> Handler RepHtml
postMarketDeleteBuyR k = do
  denyIfAnonUnPVL -- Not allowed to delete items from outside pvl as anon
  setUltDestReferer
  val <- runDB $ get404 k >>= \x -> delete k >> return x
  setMessage $ toHtml $ "Kohde " <> buyItemWhat val <> " poistettu."
  redirectUltDest MarketHomeR

postMarketDeleteSaleR :: SaleItemId -> Handler RepHtml
postMarketDeleteSaleR k = do
  denyIfAnonUnPVL -- Not allowed to delete items from outside pvl as anon
  setUltDestReferer
  val <- runDB $ get404 k >>= \x -> delete k >> return x
  setMessage $ toHtml $ "Kohde " <> saleItemName val <> " poistettu."
  redirectUltDest MarketHomeR

buyForm :: (Maybe User) -> Form BuyItem
buyForm muser = renderBootstrap $ BuyItem
  <$> areq textField  "Haluaisin ostaa:"{fsAttrs = [("list", "market-items")]} Nothing
  <*> areq intField   "Näin monta:"      (Just 1)
  <*> ((unTextarea <$>) <$> aopt textareaField "Lisätietoja:" Nothing)
  <*> areq textField  "Olen" (userUsername <$> muser)
  <*> aopt emailField "Sähköpostiosoite" (Just . userEmail <$> muser)
  <*> aopt textField  "Puhelinnumero"           (Nothing)
  <*> aopt textField  "IRC-nick"          (userIrcnick <$> muser)

sellForm :: (Maybe User) -> Form SaleItem
sellForm muser = renderBootstrap $ SaleItem
  <$> areq textField  "Myyn:"{fsAttrs = [("list", "market-items")]} Nothing
  <*> areq intField   "Näin monta:" (Just 1)
  <*> areq doubleField "Hinta (€)"  Nothing
  <*> ((unTextarea <$>) <$> aopt textareaField  "Lisätietoja:" Nothing)
  <*> areq textField  "Olen"                    (userUsername <$> muser)
  <*> aopt emailField "Sähköpostiosoite" (Just . userEmail <$> muser)
  <*> aopt textField  "Puhelinnumero"           (Nothing)
  <*> aopt textField  "IRC-nick"          (userIrcnick <$> muser)

buy, sale :: Widget
buy = do
  xs <- liftM (groupBy $ \x y -> let f = buyItemBuyer . entityVal in f x == f x) $
        liftHandlerT $ runDB $ selectList [] [Asc BuyItemBuyer, Asc BuyItemWhat]
  [whamlet|
<table>
  <thead>
    <tr>
      <th>Poista
      <th>Mitä
      <th>Yhteydenotto
  <tbody>
    $forall ys <- xs
      $forall Entity k x <- ys
        <tr>
          <td>
            <form action=@{MarketDeleteBuyR k} method=post>
              <input type="submit" value="X">
          <td>#{buyItemWhat x} #
            $if (/=) (buyItemCount x) 1
              (#{buyItemCount x} kpl) #
            $maybe desc <- buyItemDesc x
              <span .market-description>- #{desc}
          $with person <- buyItemBuyer $ entityVal $ head ys
            ^{contactWidget person (buyItemEmail x) (buyItemIrc x) (buyItemPhone x)}
  |]

contactWidget :: Text -> Maybe Text -> Maybe Text -> Maybe Text -> Widget
contactWidget person memail mirc mphone = [whamlet|
<th>#{person}: #
  <small>
    $maybe email <- memail
      <a href="mailto:#{email}">#{email}
    $maybe irc <- mirc
      , irkissä
      <i>#{irc}
    $maybe phone <- mphone
      , Puh. #{phone}
|]

sale = do
  xs <- liftHandlerT $ runDB $ selectList [] [Asc SaleItemSeller, Asc SaleItemName]
  [whamlet|
<table>
  <thead>
    <tr>
      <th>Poista
      <th>Mitä
      <th>Hinta
      <th>Yhteydenotto
  <tbody>
    $forall Entity k x <- xs
      <tr>
        <td>
          <form action=@{MarketDeleteSaleR k} method=post>
            <input type="submit" value="X">
        <td>#{saleItemName x} #
            $if (/=) (saleItemCount x) 1
              (#{saleItemCount x} kpl) #
            $maybe desc <- saleItemDesc x
              <span .market-description>- #{desc}
        <td>#{saleItemPrice x}€
        ^{contactWidget (saleItemSeller x) (saleItemEmail x) (saleItemIrc x) (saleItemPhone x)}
  |]
