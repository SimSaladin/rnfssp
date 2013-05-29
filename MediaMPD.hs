------------------------------------------------------------------------------
-- File:          MediaMPD.hs
-- Creation Date: Apr 15 2013 [22:44:04]
-- Last Modified: May 28 2013 [16:58:52]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module MediaMPD 
  ( MediaMPD
  , mediaMPD
  ) where

import Yesod
import Sections.Types

data MediaMPD master = MediaMPD master
    { mpdId       :: SectionId
    , mpdRoute    :: [Text] -> Route master
    , mpdFileRoot :: FilePath
    }

mediaMPD :: SectionId -> ([Text] -> Route master) -> MediaConf -> MPDSec master
mediaMPD sid route mc = MediaMPD sid (route) (mcPath mc)

