{-# LANGUAGE RankNTypes, CPP #-}
------------------------------------------------------------------------------
-- File:          Configs.hs
-- Creation Date: Dec 24 2012 [01:31:05]
-- Last Modified: Apr 14 2013 [15:12:32]
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
  , onSecs'
  , updateIndeces
  ) where

import Import
import Sections
import Sections.Music
import Sections.Film
import qualified Data.Map as Map

invalid :: Text -> GHandler sub master a
invalid how = invalidArgs $
  [how, "If you reached this page through a link on the website, please contact the webmaster."]

-- | Execute an action on a section.
onSec :: Section -> (forall a. MSection a => a -> Handler b) -> Handler b
onSec section f = do
    mmc <- liftM (Map.lookup section) getSections
    maybe failed (\mc -> runMediaConf section mc f) mmc
  where failed = invalid $ "Requested content not found: " `mappend` section

runMediaConf :: Section -> MediaConf -> (forall a. MSection a => a -> Handler b) -> Handler b
runMediaConf section mc f = case mcType mc of
#define g(mk) (f $ mk section mc)
        "mpd"  -> g( mkMPDSec  )
        "film" -> g( mkFilmSec )
        x      -> invalid $ "Requested section type not supported: " `mappend` x
#undef g

-- | Execute a non-Handler action on a section.
onSec' :: Section -> (forall a. MSection a => a -> b) -> Handler b
onSec' section f = onSec section (return . f)

onSecs' :: (forall s. MSection s => s -> a) -> Handler (Map.Map Section (Handler a))
onSecs' f = do
    secs <- getSections
    return $ Map.mapWithKey (\s mc -> runMediaConf s mc (return . f)) secs

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
