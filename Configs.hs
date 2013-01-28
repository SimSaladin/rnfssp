{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- File:          Configs.hs
-- Creation Date: Dec 24 2012 [01:31:05]
-- Last Modified: Jan 28 2013 [19:19:08]
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
            , ("games", "headphones")
            ]

-- |
animeContent :: FilmSec
animeContent = FilmSec "anime" "/home/media/anime" (MediaContentR "anime")

-- |
musicContent :: MPDSec
musicContent = MPDSec "music" "/home/media/music" (MediaContentR "music")

hentaiContent :: FilmSec
hentaiContent = FilmSec "hentai" "/home/media/hentai" (MediaContentR "hentai")

gamesContent :: FilmSec
gamesContent = FilmSec "games" "/home/media/game" (MediaContentR "games")

onSec :: Text -> (forall a. MSection a => a -> b) -> b
onSec sec f = case sec of
  "anime" -> f animeContent
  "music" -> f musicContent
  "hentai" -> f hentaiContent
  "games" -> f gamesContent
  _       -> error "Unknown content!"
