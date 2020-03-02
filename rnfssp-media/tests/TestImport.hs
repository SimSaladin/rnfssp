{-# LANGUAGE OverloadedStrings, GADTs, FlexibleContexts, TypeFamilies, MultiParamTypeClasses, NoImplicitPrelude, QuasiQuotes, TemplateHaskell, DeriveDataTypeable #-}
module TestImport
    ( module X
    , module TestImport
    ) where

import Yesod.Test as X
import MediaSub()
import MediaSub.Standalone
import MediaSub.Data as X hiding (Widget, Handler, get, runDB)
import Database.Persist.Sql
import Database.Persist.Sqlite

type Spec = YesodSpec TestApp

type PersistConf = SqliteConf

runDB :: SqlPersistM a -> YesodExample TestApp a
runDB query = do
    pool <- fmap connPool getTestYesod
    liftIO $ runSqlPersistMPool query pool
