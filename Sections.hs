{-# LANGUAGE ExistentialQuantification, StandaloneDeriving #-}
------------------------------------------------------------------------------
-- File:          Sections.hs
-- Creation Date: Dec 23 2012 [23:10:22]
-- Last Modified: Apr 06 2013 [21:57:57]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections (MSection(..)) where

import Import

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
