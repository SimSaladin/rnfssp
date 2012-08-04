{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Model where

import Prelude
import Yesod
import Yesod.Markdown
import Yesod.Auth.HashDB (HashDBUser(..))
import Data.Aeson.Types (ToJSON(..), FromJSON(..), (.:))
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Database.Persist.Quasi

type TupleText = (Text, Text)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser (UserGeneric backend) where
    userPasswordHash = Just . userPassword
    userPasswordSalt = Just . userSalt
    setSaltAndPasswordHash s h p = p { userSalt     = s
                                     , userPassword = h
                                     }

instance ToJSON Playlist where
    toJSON (Playlist title owner elems create modified) = object
        [ "title" .= title
        , "owner" .= owner
        , "elems" .= elems
        , "created" .= create
        , "modified" .= modified
        ]

instance FromJSON Playlist where
  parseJSON (Object v) = Playlist <$> v .: "title"
                                  <*> v .: "owner"
                                  <*> v .: "elems"
                                  <*> v .: "created"
                                  <*> v .: "modified"
  parseJSON          _ = undefined
