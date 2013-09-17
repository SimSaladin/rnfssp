------------------------------------------------------------------------------
-- File:          Sections/Types.hs
-- Creation Date: Apr 15 2013 [22:38:30]
-- Last Modified: Sep 17 2013 [05:08:04]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | The base for Media sections.
module Sections.Types where

import           Prelude
import           Data.Text (Text)
import           Data.Conduit
import           Control.Monad
import           Control.Applicative
import           Yesod

-- * Configuration types

-- | Identifier of a section.
type SectionId = Text
type FPS = [FilePath]

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

-- | Wrapper class to ease type signatures
class ( MediaBrowsable  app source
      , MediaUpdate     app source
      , MediaSearchable app source
      , ToJSON (MElem source))
      => MyMedia app source where

-- * Backends

-- | Browsable media. 
class MediaBrowsable app source where
    data MElem source :: *

    -- | Fetch elements at FPS in JSON format
    browsableFetchElems  :: (ToJSON (MElem source))
                         => FPS
                         -> source
                         -> Source (HandlerT app IO) (MElem source)

    browsableRender :: Source (HandlerT app IO) (MElem source)
                    -> FPS
                    -> source
                    -> WidgetT app IO ()

    browsableFetchPlain :: FPS -> source -> HandlerT app IO FilePath

    -- | Fetch plain elements recursively.
    browsableFetchPlainR :: FPS
                         -> source
                         -> Source (HandlerT app IO) FilePath

    -- | Provide a banner widget to the media.
    -- XXX: Add default implemantion
    browsableBanner :: source -> WidgetT app IO ()

    -- | Define the JS function used to render the content client side.
    -- TODO: details?
    browsableJSRender :: Text -> source -> WidgetT app IO ()

browsableFetchWidget :: (ToJSON (MElem source), MediaBrowsable app source)
                     => FPS -> source -> WidgetT app IO ()
browsableFetchWidget fps source = browsableRender (browsableFetchElems fps source) fps source

class MediaUpdate app source where
    updateMedia :: source -> HandlerT app IO [(FPS, Html)]

-- | Search interface to a media source.
class MediaBrowsable app source => MediaSearchable app source where

    -- | The datatype for an advanced search.
    data MSearch source :: *

    -- | Clientside javascript `function identifier(data, dom)`, which renders
    -- search results from `data` in `dom`.
    searchableJSRender  :: Text -> source
                        -> WidgetT app IO ()

    -- | Advanced search form.
    searchableForm    :: source -> AForm (HandlerT app IO) (MSearch source)

    -- | Text search.
    searchableSearchT :: Text -> source
                      -> Source (HandlerT app IO) (MElem source)

    -- | Advanced search.
    searchableSearch  :: MSearch source -> source
                      -> Source (HandlerT app IO) (MElem source)

