------------------------------------------------------------------------------
-- File:          Sections.hs
-- Creation Date: Dec 23 2012 [23:10:22]
-- Last Modified: Dec 26 2012 [18:51:40]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections (MSection(..)) where

import Import

class MSection a where
  
  -- | Name of the section
  ident :: a -> Text

  -- | Find stuff to add on a specific path
  findr :: a -> Text -> Handler [Text]

  -- | Content widget for a path
  content :: a -> [Text] -> Widget

  -- | Get results by a search query
  searchContent :: a -> Text -> Widget

  -- | Resolve to a real file in FS.
  filepath :: a -> Text -> Handler FilePath

  -- | Action which updates index of the section.
  updateIndex :: a -> Handler ()
