------------------------------------------------------------------------------
-- File:          Sections.hs
-- Creation Date: Dec 23 2012 [23:10:22]
-- Last Modified: Dec 24 2012 [15:47:18]
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

  -- | 
  filepath :: a -> Text -> Handler FilePath

  -- | Action which updates index of the section.
  updateIndex :: a -> Handler ()
