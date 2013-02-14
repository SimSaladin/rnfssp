{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- File:          Configs.hs
-- Creation Date: Dec 24 2012 [01:31:05]
-- Last Modified: Feb 13 2013 [20:56:31]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Configs
  ( module Sections
  , renderBrowsable
  , onSec
  , onSec'
  , updateIndeces
  ) where

import Import
import Sections
import Sections.Music
import Sections.Film
import qualified Data.Map as Map
import qualified Data.Text as T
import Data.Maybe (fromJust)

onSec :: Text -> (forall a. MSection a => a -> Handler b) -> Handler b
onSec ident f = do
    mmc <- liftM (Map.lookup ident . extraSections) getExtra -- TODO: too much overhead?
    -- let f' sec = f $ sec ident (mcPath $ fromJust mmc) (MediaContentR ident)
    case mcType <$> mmc of
        Just "mpd"  -> f $ MPDSec  ident (mcPath $ fromJust mmc) (MediaContentR ident)
        Just "film" -> f $ FilmSec ident (mcPath $ fromJust mmc) (MediaContentR ident)
        Just x      -> error $ "Requested content type \"" ++ T.unpack x ++ "\" not supported."
        Nothing     -> error $ "Requested content \"" ++ T.unpack ident ++ "\" not found."

onSec' :: Text -> (forall a. MSection a => a -> b) -> Handler b
onSec' ident f = onSec ident (return . f)

updateIndeces :: Handler ()
updateIndeces = liftM (Map.keys . extraSections) getExtra
    >>= mapM_ (\x -> onSec x sUpdateIndex)

-- | XXX: convert to renderBrowsable
browsable' :: Handler [(Text, Text, Text)]
browsable' = liftM (Map.elems . Map.mapWithKey f . extraSections) getExtra
  where f key mc = (key, mcView mc, mcIcon mc)

renderBrowsable :: Text -> Widget
renderBrowsable current = do
    elements <- lift browsable'
    [whamlet|$newline never
<div .pagination>
  <ul>
    $forall (ident, view, icon) <- elements
      $if current == ident
        <li .active>
          <a href=@{f ident}>
            <i .icon-white .icon-#{icon}>
            &nbsp;#{view}
      $else
        <li>
          <a href=@{f ident}>
            <i .icon-white .icon-#{icon}>
            &nbsp;#{view}
    |] where f = flip MediaContentR []
