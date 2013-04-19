------------------------------------------------------------------------------
-- File:          MediaMPD.hs
-- Creation Date: Apr 15 2013 [22:44:04]
-- Last Modified: Apr 17 2013 [21:17:26]
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
mediaMPD sid mc = MediaMPD sid (MediaContentR sid) (mcPath mc)

