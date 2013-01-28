{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- File:          Configs.hs
-- Creation Date: Dec 24 2012 [01:31:05]
-- Last Modified: Dec 31 2012 [08:14:48]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Configs ( module Sections, browsable, onSec ) where

import Import
import Sections
import Sections.Music
import Sections.Film

browsable :: [(Text, Text)]
browsable = [ ("anime", "film")
            , ("music", "music")
            , ("hentai", "film")
            ]

-- |
animeContent :: FilmSec
animeContent = FilmSec "anime" "/home/media/anime" (MediaContentR "anime")

-- |
musicContent :: MPDSec
musicContent = MPDSec "music" "/home/media/music" (MediaContentR "music")

hentaiContent :: FilmSec
hentaiContent = FilmSec "hentai" "/home/media/hentai" (MediaContentR "hentai")

onSec :: Text -> (forall a. MSection a => a -> b) -> b
onSec sec f = case sec of
  "anime" -> f animeContent
  "music" -> f musicContent
  "hentai" -> f hentaiContent
  _       -> error "Unknown content!"
