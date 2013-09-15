{-# LANGUAGE RankNTypes, CPP #-}
------------------------------------------------------------------------------
-- File:          Configs.hs
-- Creation Date: Dec 24 2012 [01:31:05]
-- Last Modified: Sep 15 2013 [05:36:38]
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
  , wrapMain
  ) where

import Import hiding (update)
import Sections
import Sections.Music
import Sections.Film
import Sections.Types
import Sections.BackendGitAnnex
import qualified Data.Map as Map

invalid :: Text -> HandlerT master IO a
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
        "mpd"   -> g( mkMPDSec  )
        "film"  -> g( mkFilmSec )
        "annex" -> g( mkGABE    )
        x      -> invalid $ "Requested section type not supported: " `mappend` x
#undef g

-- | Execute a non-Handler action on a section.
onSec' :: Section -> (forall a. MSection a => a -> b) -> Handler b
onSec' section f = onSec section (return . f)

onSecs' :: (forall s. MSection s => s -> a) -> Handler (Map.Map Section (Handler a))
onSecs' f = do
    secs <- getSections
    return $ Map.mapWithKey (\s mc -> runMediaConf s mc (return . f)) secs

updateIndeces :: Handler [(SectionId, [RecentlyAdded])]
updateIndeces = do
    mcs <- getSections
    sequence . Map.elems $ Map.mapWithKey update mcs
  where
      update sid mc = do added <- runMediaConf sid mc sUpdateIndex
                         return (sid, added)

getSections :: Handler (Map.Map Section MediaConf)
getSections = liftM extraSections getExtra
{-# INLINE getSections #-}

-- | XXX: convert to renderBrowsable
browsable' :: Handler [(Section, Text, Text)]
browsable' = liftM (Map.elems . Map.mapWithKey f . extraSections) getExtra
  where f key mc = (key, mcView mc, mcIcon mc)

renderBrowsable :: Section -> Widget
renderBrowsable current = do
    elements <- liftHandlerT browsable'
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
    sections <- liftHandlerT getSections
    [whamlet|
$forall x <- sections
  #{show x}
|]

wrapMain :: Widget -> Widget
wrapMain w = [whamlet|<main>^{w}|]
