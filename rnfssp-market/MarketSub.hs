{-# LANGUAGE TemplateHaskell, QuasiQuotes, RankNTypes, FlexibleInstances, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, ConstraintKinds, TypeFamilies #-}

-- | A simple market subsite for yesod sites. Allows users to list items
-- for sale and set buy requests.
--
-- Usage example:
--
--  Add a master route:
--
--      /market SubsiteR MarketSub getMarketSub
--
-- Add database migration:
--  
--      runMigration migrateMarket
--
module MarketSub
    ( module MarketSub.Data
    , module MarketSub
    ) where

import MarketSub.Data
import Yesod
import Database.Persist.Sql (SqlPersistT)
import Control.Applicative
import Data.Monoid
import Data.Text (Text)

class ( Yesod m, RenderMessage m FormMessage
      , YesodPersist m, YesodPersistBackend m ~ SqlPersistT
      ) => MarketSubClass m where
    checkDeleteRights :: HandlerT m IO ()
    marketHeader :: WidgetT m IO ()

type Handler a = forall master. MarketSubClass master
               => HandlerT MarketSub (HandlerT master IO) a

type Widget = forall master. MarketSubClass master => WidgetT master IO ()

type Form m x = Html -> MForm (HandlerT m IO) (FormResult x, WidgetT m IO ())

instance MarketSubClass master => YesodSubDispatch MarketSub (HandlerT master IO)
        where
            yesodSubDispatch = $(mkYesodSubDispatch resourcesMarketSub)

-- * Handlers

getMarketHomeR :: Handler Html
getMarketHomeR = do
    (buyWidget, buyerEnctype)   <- lift $ generateFormPost $ listingForm False
    (sellWidget, sellerEnctype) <- lift $ generateFormPost $ listingForm True
    items <- lift getItemsList
    toMaster <- getRouteToParent
    lift $ defaultLayout $ [whamlet|
<main>
   ^{marketHeader}
   <div .ym-grid .ym-equalize .linearize-level-1>
      <div .ym-g50 .ym-gl>
         <section .site-block-h>
            <h1>Ostetaan
            ^{viewBuys toMaster}
         <section .site-block-h>
            <h1>Uusi ostoilmoitus
            <form .form-horizontal method=post action=@{toMaster $ MarketAddListingR "buy"} enctype=#{buyerEnctype}>
               ^{buyWidget}
               <div .form-actions>
                  <input type="submit" value="Lisää ostoilmoitus">
      <div .ym-g50 .ym-gr>
         <section .site-block-h>
            <h1>Myydään
            ^{viewSales toMaster}
         <section .site-block-h>
            <h1>Uusi myynti-ilmoitus
            <form .form-horizontal method=post action=@{toMaster $ MarketAddListingR "sell"} enctype=#{sellerEnctype}>
               ^{sellWidget}
               <div .form-actions>
                  <input type="submit" value="Lisää myynti-ilmoitus">
   <datalist #market-items>
      $forall Entity _ item <- items
         <option value=#{marketListingWhat item}>
   <aside .site-block>
      <p>Tämä sivu on nähtävissä kaikkialta. #
         <b>Poistotoiminnot vain Päivölästä tai sisäänkirjautuneille. #
      <p>
         Bugeja ja parannusehdotuksia otetaan vastaan irkissä #
         (bps@PVLnet) tai sähköpostilla
         <a href="mailto:simsaladin -ät- paivola.fi">simsaladin -ät- paivola.fi
|]

postMarketAddListingR :: Text -> Handler Html
postMarketAddListingR kind = do
    kind' <- case kind of
                 "sell" -> return True
                 "buy"  -> return True
                 _      -> notFound
    ((res, _), _) <- lift $ runFormPost $ listingForm kind'
    handleItem res

postMarketDelListingR :: MarketListingId -> Handler Html
postMarketDelListingR k = do
  lift checkDeleteRights
  setUltDestReferer
  val <- lift $ runDB $ get404 k >>= \x -> delete k >> return x
  setMessage $ toHtml $ "Kohde " <> marketListingWhat val <> " poistettu."
  redirectUltDest MarketHomeR

getItemsList :: MarketSubClass m => HandlerT m IO [Entity MarketListing]
getItemsList = runDB $ selectList [] []

handleItem :: FormResult MarketListing -> Handler Html
handleItem result = do
    setUltDestReferer
    case result of
        FormMissing   -> setMessage "Tiedot olivat tyhjiä!" >> redirectUltDest MarketHomeR
        FormFailure _ -> setMessage "Ilmoituksen lisääminen ei onnistunut." >> getMarketHomeR
        FormSuccess item -> do
            _ <- lift $ runDB $ insert item
            let message name = if marketListingSale item
                                then "Kohde " <> name <> " lisätty myyntiin."
                                else "Osto-ilmoitus " <> name <> " lisätty."
                in setMessage $ toHtml $ message (marketListingWhat item)
            redirectUltDest MarketHomeR

-- * Forms and widgets

listingForm :: RenderMessage m FormMessage => Bool -> Form m MarketListing
listingForm sale = renderBootstrap $ MarketListing sale
  <$> areq textField   whatSettings Nothing
  <*> areq intField    "Näin monta:" (Just 1)
  <*> areq doubleField "Hinta (€)"   Nothing
  <*> (fmap unTextarea <$> aopt textareaField "Lisätietoja:" Nothing)
  <*> contactForm
  where
    whatSettings =
        (if sale then "Myyn:" else "Ostan:") {fsAttrs = [("list", "market-items")]}
    contactForm = MarketContact
      <$> areq textField  "Olen"             Nothing
      <*> aopt emailField "Sähköpostiosoite" Nothing
      <*> aopt textField  "Puhelinnumero"    Nothing
      <*> aopt textField  "IRC-nick"         Nothing

viewBuys :: MarketSubClass m => (Route MarketSub -> Route m) -> WidgetT m IO ()
viewBuys toMaster = do
    buys <- handlerToWidget $ runDB $ selectList [] [Asc MarketListingContact, Asc MarketListingWhat]
    wrapListing toMaster buys

viewContact :: MarketContact -> Widget
viewContact contact = [whamlet|
#{marketContactName contact}: #
<small>
    $maybe email <- marketContactEmail contact
      <a href="mailto:#{email}">#{email}
    $maybe irc <- marketContactIrc contact
      , irkissä
      <i>#{irc}
    $maybe phone <- marketContactPhone contact
      , Puh. #{phone}
|]

viewSales :: MarketSubClass m => (Route MarketSub -> Route m) -> WidgetT m IO ()
viewSales toMaster = do
    sales <- handlerToWidget $ runDB $ selectList [] [Asc MarketListingContact, Asc MarketListingWhat]
    wrapListing toMaster sales

wrapListing :: MarketSubClass m => (Route MarketSub -> Route m) -> [Entity MarketListing] -> WidgetT m IO ()
wrapListing toMaster items = [whamlet|
<table>
  <thead>
    <tr>
      <th>Poista
      <th>Mitä
      <th>Hinta
      <th>Yhteydenotto
  <tbody>
    $forall Entity k item <- items
      <tr>
        <td>
          <form .bare action=@{toMaster $ MarketDelListingR k} method=post>
            <input type="submit" value="X">
        <td>#{marketListingWhat item} #
            $if (/=) (marketListingCount item) 1
              (#{marketListingCount item} kpl) #
            $maybe desc <- marketListingDesc item
              <span .market-description>- #{desc}
        <td>#{marketListingPrice item}€
        <th>^{viewContact (marketListingContact item)}
|]
