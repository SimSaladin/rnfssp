{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
------------------------------------------------------------------------------
-- File:          Sections.hs
-- Creation Date: Dec 23 2012 [23:10:22]
-- Last Modified: Apr 13 2013 [18:18:03]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections (MSection(..), Section) where

import Import

type Section = Text

class MSection a where

  -- | Find stuff to add on a specific path
  sFind :: a -> [Text] -> Handler [Text]

  -- | Content widget for a path
  sWContent :: a -> [Text] -> Widget

  -- | Get results by a search query
  sWSearch :: a -> Text -> Widget

  -- | Resolve to a real file in FS.
  sFilePath :: a -> Text -> Handler FilePath

  -- | Action which updates index of the section.
  sUpdateIndex :: a -> Handler ()

  -- | Get path to a large icon for the section.
  sLargeIcon :: a -> FilePath
  sLargeIcon _ = ""
