{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- File:          Configs.hs
-- Creation Date: Dec 24 2012 [01:31:05]
-- Last Modified: Jan 14 2013 [21:27:40]
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
            , ("movies", "film")
            , ("books", "book")
            ]

-- |
animeContent :: FilmSec
animeContent = FilmSec "anime" "/home/media/anime" (MediaContentR "anime")

-- |
musicContent :: MPDSec
musicContent = MPDSec "music" "/home/media/music" (MediaContentR "music")

hentaiContent :: FilmSec
hentaiContent = FilmSec "hentai" "/home/media/hentai" (MediaContentR "hentai")

moviesContent :: FilmSec
moviesContent = FilmSec "movies" "/home/media/movie" (MediaContentR "movies")

booksContent :: FilmSec
booksContent = FilmSec "books" "/home/media/books" (MediaContentR "books")

onSec :: Text -> (forall a. MSection a => a -> b) -> b
onSec sec f = case sec of
  "anime"  -> f animeContent
  "music"  -> f musicContent
  "hentai" -> f hentaiContent
  "movies" -> f moviesContent
  "books"  -> f booksContent
  _        -> error "Unknown content!"

