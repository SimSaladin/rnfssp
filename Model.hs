module Model where

import Prelude
import Yesod
import Yesod.Auth.HashDB (HashDBUser(..))
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
