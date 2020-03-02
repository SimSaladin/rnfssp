{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Model where

import Prelude
import Yesod
import Yesod.Markdown
import Yesod.Auth.HashDB (HashDBUser(..))
--import Data.Aeson.Types (ToJSON(..), FromJSON(..), (.:))
import Control.Applicative ((<$>), (<*>))
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Map (Map)
import Database.Persist.Quasi
import Data.Typeable (Typeable)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance HashDBUser User where
    userPasswordHash = Just . userPassword
    userPasswordSalt = Just . userSalt
    setSaltAndPasswordHash s h p = p { userSalt     = s
                                     , userPassword = h
                                     }
