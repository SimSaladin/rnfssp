-- | Use the contents of a directory as the source for a section.
--
-- On startup: 
--  fork a process, which:
--      * syncs the index from the directory.
--      * watches the directory for changes via inotify, and sync changes to the
--        index.
--      * 
--
module Sections.SingleSource (SingleSource(..)) where

import Sections
import Import
import Utils

-- Ain't this supposed to be a subsite?

-- | Server-side configuration object.
data SingleSource = SingleSource
    { sName  :: Text
    , sPath  :: FilePath
    , sRoute :: [Text] -> Route App
    }

-- | Configuration object from client.
data SSViewConf = SSViewConf
    { cPerPage :: Int
    , cPage    :: Int
    }

instance MSection SingleSource where
    ident   = sName
    content = renderContent



renderContent :: SingleSource -> [Text] -> Widget
renderContent SingleSource{sPath = fpath} fps = do
    -- get path in fpath
    -- render sources
