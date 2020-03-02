{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, FlexibleContexts, GADTs, OverloadedStrings, EmptyDataDecls #-}

module MarketSub.Data where

import Yesod
import Data.Text (Text)

data MarketSub = MarketSub

share [mkPersist sqlSettings, mkMigrate "migrateMarket"] [persistUpperCase|
MarketContact
    name  Text
    email Text Maybe
    phone Text Maybe
    irc   Text Maybe
    UniqueMarketContact name
MarketListing
   sale     Bool
   what     Text
   count    Int default=1
   price    Double
   desc     Text Maybe
   contact  MarketContact
SaleMessage
   from MarketContactId
   to   MarketContactId
   msg  Text
|]


mkYesodSubData "MarketSub" [parseRoutes|
/                     MarketHomeR         GET
/add/#Text            MarketAddListingR   POST
/del/#MarketListingId MarketDelListingR   POST
|]
