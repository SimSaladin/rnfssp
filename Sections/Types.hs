------------------------------------------------------------------------------
-- File:          Sections/Types.hs
-- Creation Date: Apr 15 2013 [22:38:30]
-- Last Modified: May 28 2013 [12:50:56]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | The base for Media sections.
module Sections.Types where

import           Prelude
import           Data.Text (Text)
import           Control.Monad
import           Control.Applicative
import           Yesod
import           Data.Aeson

-- * Configuration types

-- | Defines settings for a section. Section implementations are constructed
-- from these.
data MediaConf = MediaConf
    { mcType :: Text      -- ^ Type determines the section implemntation.
    , mcView :: Text      -- ^ Screen name for the section.
    , mcIcon :: Text      -- ^ Icon to use: static/img/<mcIcon a>.
    , mcPath :: FilePath  -- ^ Path for the section implementation.
    } deriving Show

instance FromJSON MediaConf where
    parseJSON (Object o) = MediaConf
      <$> o .: "type"
      <*> o .: "view"
      <*> o .: "icon"
      <*> o .: "path"
    parseJSON _ = mzero

-- | Identifier for a section.
type SectionId = Text

-- | Media search
class Monad master => MediaBrowsable master source where

    -- | Get JSON data to source.
    browsableContent     :: ToJSON a => [Text] -> source -> HandlerT (HandlerSite master) IO a

    -- | For defining Javascript function <identifier>(data) which returns dom
    -- element with the content.
    browsableRenderer    :: Text -> source -> WidgetT (HandlerSite master) IO ()

    -- | Description widget.
    browsableDescription :: source -> WidgetT (HandlerSite master) IO ()

    -- | Finding items for adding to the playlist.
    -- XXX: playlist 
    browsableFindElems   :: [Text] -> source -> HandlerT (HandlerSite master) IO [Text]

-- | Search interface to a media source.
class MediaBrowsable master source => MediaSearchable master source where

    -- | The datatype for an advanced search.
    type Search

    -- | Clientside javascript `function identifier(data, dom)`, which renders
    -- search results from `data` in `dom`.
    searchableRender  :: Text -> source -> WidgetT (HandlerSite master) IO ()

    -- | Advanced search form.
    searchableForm    :: source -> AForm master Search

    -- | Text search.
    searchableSearchT :: ToJSON a => Text   -> source -> HandlerT (HandlerSite master) IO a

    -- | Advanced search.
    searchableSearch  :: ToJSON a => Search -> source -> HandlerT (HandlerSite master) IO a

