{-# LANGUAGE CPP, ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Sections
   ( module Sections.Types
   , onSection
   , onSections
   , renderBrowsable
   , updateAllMedia
   ) where

import qualified Data.Map as Map
import Import
import Utils

import Sections.Types
import Sections.Music
import Sections.BackendGitAnnex
--import Sections.Film

instance ToJSON (MElem MPDSec) where
    toJSON = undefined

instance MyMedia App GitAnnexBackend
instance MyMedia App MPDSec

-- | Execute an action on a section.
onSection :: SectionId
          -> (forall a. MyMedia App a => a -> b)
          -> Handler b
onSection section f = do
    mmc <- liftM (Map.lookup section) getSections
    maybe failed (\mc -> runMediaConf section mc f) mmc
  where failed = invalid $ "Requested content not found: " <> section

onSections :: (forall s. MyMedia App s => s -> a)
           -> Handler (Map.Map SectionId (Handler a))
onSections f = do
    secs <- getSections
    return $ Map.mapWithKey (\s mc -> runMediaConf s mc f) secs

-- | Take a MediaConf and run an action on it.
runMediaConf :: SectionId -> MediaConf
             -> (forall a. MyMedia App a => a -> b)
             -> Handler b
runMediaConf section mc f = case mcType mc of
#define g(mk) (return $ f $ mk section mc)
        "mpd"   -> g( mkMPDSec  )
--      "film"  -> g( mkFilmSec )
        "annex" -> g( mkGABE    )
        x -> invalid $ "Requested section type not supported: " <> x
#undef g

-- * Navigation

renderBrowsable :: SectionId -- ^ Currently selected section
                -> Widget
renderBrowsable cur = do
    bs <- liftHandlerT sectionBanners
    [whamlet|$newline never
<nav .subnavbar>
  <ul>
    $forall (s, w) <- bs
        <li :cur == s:.active>
          <a href=@{f s}>^{w}
    |] where
        f = flip MediaContentR []

-- * Helper functions

getSections :: Handler (Map.Map SectionId MediaConf)
{-# INLINE getSections #-}
getSections = liftM extraSections getExtra

updateAllMedia :: Handler [RecentlyAdded]
updateAllMedia = getSections >>= liftM join . sequence . Map.elems . Map.mapWithKey f
    where f s mc = do
            xs <- join $ runMediaConf s mc
                     (updateMedia :: forall s. MyMedia App s => s -> Handler [(FPS, Html)])
            time <- timeNow
            return $ map (uncurry $ RecentlyAdded time s) (xs :: [(FPS, Html)])

sectionBanners :: Handler [(SectionId, Widget)]
sectionBanners = do
    banners <- onSections browsableBanner
    sequence $ Map.elems $ Map.mapWithKey (\k -> liftM (k,)) banners

invalid :: Text -> HandlerT master IO a
invalid how = invalidArgs $
    [how, "If you reached this page through a link on the website, please contact the webmaster."]
