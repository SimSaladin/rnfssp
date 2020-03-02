{-# LANGUAGE NoImplicitPrelude, RankNTypes, TemplateHaskell, QuasiQuotes, OverloadedStrings, ConstraintKinds, FlexibleInstances, FlexibleContexts, TypeFamilies, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | A very basic site to use the subsite as is.
module MediaSub.Standalone
    ( makeApplication
    , makeFoundation
    , getApplicationDev
    ) where

import MediaSub()
import MediaSub.Data
import MediaSub.Import
import MediaSub.Music
import MediaSub.PlainAnnex

import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Default
import Yesod.Default.Main
import Yesod.Default.Config
import System.Log.FastLogger (newStdoutLoggerSet, defaultBufSize)
import Network.Wai.Middleware.RequestLogger
    ( mkRequestLogger, outputFormat, OutputFormat (..), destination
    )
import Network.Wai.Logger (clockDateCacher)
import Yesod.Core.Types (loggerSet, Logger (Logger))
import qualified Network.Wai.Middleware.RequestLogger as RequestLogger

-- * Boring stuff

type PersistConf = SqliteConf

data TestApp = TestApp
             { getMediaSubR :: MediaSub
             , connPool :: PersistConfigPool PersistConf
             , persistConfig :: PersistConf
             , appLogger :: Logger
             }

share [mkPersist sqlSettings, mkMigrate "migrateMediaTest"] [persistUpperCase|
User
    name Text
    UniqueUser name
    deriving Typeable
|]

instance YesodAuth TestApp where
    type AuthId TestApp = UserId

instance RenderMessage TestApp FormMessage where

-- * Interesting stuff

instance YesodMediaSub TestApp where
    mediaSections = buildSections
        [("music", music 6600 "localhost" "")
        ,("anime", plainAnnex "/home/media/anime")
        ]

-- * More boring stuff

mkYesod "TestApp" [parseRoutes|
/ MediaSubR MediaSub getMediaSubR
|]

instance Yesod TestApp where

instance YesodPersist TestApp where
    type YesodPersistBackend TestApp = SqlPersistT

instance YesodPersistRunner TestApp where
    getDBRunner = defaultGetDBRunner connPool

-- * Enter points

makeFoundation :: IO TestApp
makeFoundation = do
    let dbconf = SqliteConf "tests.sqlite" 5
    pool <- createSqlitePool "tests.sqlite" 5

    loggerSet' <- newStdoutLoggerSet defaultBufSize
    (getter, _) <- clockDateCacher
    let logger = Yesod.Core.Types.Logger loggerSet' getter

    return $ TestApp def pool dbconf logger

makeApplication :: IO Application
makeApplication = do
    foundation <- makeFoundation

    logWare <- mkRequestLogger def
        { outputFormat = Detailed True
        , destination = RequestLogger.Logger $ loggerSet $ appLogger foundation
        }

    app <- toWaiAppPlain foundation
    return $ logWare app

-- for yesod devel
getApplicationDev :: IO (Int, Application)
getApplicationDev =
    defaultDevelApp (return undefined :: IO (AppConfig Text Text)) (const makeApplication)

