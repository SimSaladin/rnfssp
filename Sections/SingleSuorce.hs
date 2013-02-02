------------------------------------------------------------------------------
-- File:          Sections/SingleSuorce.hs
-- Creation Date: Jan 28 2013 [19:03:51]
-- Last Modified: Jan 28 2013 [22:01:17]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections.SingleSource (SingleSource(..)) where

import Sections
import Import
import Utils

data SingleSource = SingleSource { sName  :: Text
                                 , sPath  :: FilePath
                                 , sRoute :: [Text] -> Route App
                                 }


instance MSection SingleSource where
  ident = sName
  content = renderContent

renderContent :: SingleSource -> [Text] -> Widget
renderContent SingleSource{sPath = fpath} fps = do
  -- get path in fpath
  -- render sources
