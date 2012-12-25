{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- File:          Configs.hs
-- Creation Date: Dec 24 2012 [01:31:05]
-- Last Modified: Dec 25 2012 [00:14:55]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Configs ( module Sections, onSec ) where

import Import
import Sections
import Sections.Music
import Sections.Film

-- |
animeContent :: FilmSec
animeContent = FilmSec "anime" "/home/sim/anime" (MediaContentR "anime")

-- |
musicContent :: MPDSec
musicContent = MPDSec "music" (MediaContentR "music") "/home/sim/music"

onSec :: Text -> (forall a. MSection a => a -> b) -> b
onSec sec f = case sec of
  "anime" -> f animeContent
  "music" -> f musicContent
  _       -> error "Unknown content!"
