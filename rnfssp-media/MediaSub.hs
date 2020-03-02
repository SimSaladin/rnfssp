{-# LANGUAGE NoImplicitPrelude, RankNTypes, TemplateHaskell, QuasiQuotes, OverloadedStrings, ConstraintKinds, FlexibleInstances, FlexibleContexts, TypeFamilies, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Rnfssp media subsite
module MediaSub 
    ( module MediaSub
    , module MediaSub.Data
    ) where

import Prelude
import Yesod
import Database.Persist.Sql (SqlPersistT)

import MediaSub.Data
-- Handlers
import MediaSub.Media
import MediaSub.Playlist
-- Sections
import MediaSub.Music
import MediaSub.PlainAnnex

instance YesodMediaSub master => YesodSubDispatch MediaSub (HandlerT master IO)
        where
            yesodSubDispatch = $(mkYesodSubDispatch resourcesMediaSub)
