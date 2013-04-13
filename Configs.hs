{-# LANGUAGE RankNTypes, CPP #-}
------------------------------------------------------------------------------
-- File:          Configs.hs
-- Creation Date: Dec 24 2012 [01:31:05]
-- Last Modified: Apr 13 2013 [18:26:47]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
--
-- | Media Sections configuration.
module Configs
  ( module Sections
  , renderBrowsable
  , getSections
  , sectionsBlockNav
  , onSec
  , onSec'
  , updateIndeces
  ) where

import Import
import Sections
import Sections.Music
import Sections.Film
import qualified Data.Map as Map
import Data.Maybe (fromJust)

-- | Execute an action on a section.
onSec :: Section -> (forall a. MSection a => a -> Handler b) -> Handler b
onSec section f = do
    mmc <- liftM (Map.lookup section) getSections
#define g(mk) (f $ mk section $ fromJust mmc)
    case mcType <$> mmc of
        Just "mpd"  -> g( mkMPDSec  )
        Just "film" -> g( mkFilmSec )
        Just x      -> invalid $ "Requested section type not supported: " `mappend` x
        Nothing     -> invalid $ "Requested content not found: "          `mappend` section
#undef g
  where
    invalid how = invalidArgs $
      [how, "If you reached this page through a link on the website, please contact the webmaster."]

-- | Execute a non-Handler action on a section.
onSec' :: Section -> (forall a. MSection a => a -> b) -> Handler b
onSec' section f = onSec section (return . f)

updateIndeces :: Handler ()
updateIndeces = liftM (Map.keys . extraSections) getExtra
    >>= mapM_ (\x -> onSec x sUpdateIndex)

getSections :: Handler (Map.Map Section MediaConf)
getSections = liftM extraSections getExtra
{-# INLINE getSections #-}

-- | XXX: convert to renderBrowsable
browsable' :: Handler [(Section, Text, Text)]
browsable' = liftM (Map.elems . Map.mapWithKey f . extraSections) getExtra
  where f key mc = (key, mcView mc, mcIcon mc)

renderBrowsable :: Section -> Widget
renderBrowsable current = do
    elements <- lift browsable'
    [whamlet|$newline never
<nav .subnavbar>
  <ul>
    $forall (ident, view, icon) <- elements
        <li :current == ident:.active>
          <a href=@{f ident}>
            <i .icon-white .icon-#{icon}>
            &nbsp;#{view}
    |] where f = flip MediaContentR []

sectionsBlockNav :: Widget
sectionsBlockNav = do
    sections <- lift getSections
    [whamlet|
$forall x <- sections
  #{show x}
|]
