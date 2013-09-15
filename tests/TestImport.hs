{-# LANGUAGE OverloadedStrings #-}
module TestImport
    ( module Yesod.Test
    , runDB
    , Spec
    , Example
    ) where

import Yesod.Test
import Database.Persist.Sql
import Control.Monad.IO.Class (liftIO)

import Foundation
import Model

type Spec = YesodSpec App
type Example = YesodExample App

runDB :: SqlPersistM a -> Example a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool
