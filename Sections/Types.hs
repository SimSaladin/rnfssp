------------------------------------------------------------------------------
-- File:          Sections/Types.hs
-- Creation Date: Apr 15 2013 [22:38:30]
-- Last Modified: Oct 03 2013 [02:21:02]
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

-- * Configuration API

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

-- * Backends API

-- | The listing of some view
data ListContent sec where
    ListSingle  :: sec -> FPS ->                  MElem sec  -> ListContent sec
    ListFlat    :: sec -> FPS -> MediaSource app (MElem sec) -> ListContent sec
    ListBlocks  :: sec -> FPS -> MediaSource app (MElem sec) -> ListContent sec

type Paging            = Maybe (Int, Int)
type MediaSource app a = Source (HandlerT app IO) a
type MediaAForm  app a = AForm  (HandlerT app IO) a
type MediaView   app a = HandlerT app IO (ListContent a)

-- | Browsable media. 
class MediaBrowsable app source where
    data MElem source :: *

    -- | Provide a banner widget to the media.
    browsableBanner      :: {- Bool -> -} source -> WidgetT app IO () -- XXX: Add default implemantion

    -- | Fetch elements at FPS in JSON format
    browsableFetchElems  :: ToJSON (MElem source)
                         => FPS -> source -> Paging -> MediaView app source

    -- | Fetch a single element. Should fail if it is not found.
    browsableFetchPlain  :: FPS -> source -> HandlerT app IO FilePath

    -- | Fetch plain elements recursively.
    browsableFetchPlainR :: FPS -> source -> MediaSource app FilePath

    -- | Define the JS function used to render the content client side.
    browsableJSRender    :: Text -> source -> WidgetT app IO () -- TODO: details?

    browsableServerRender :: ListContent source -> WidgetT app IO ()
    browsableServerRender _ = [whamlet|default view!|]

class MediaUpdate app source where
    updateMedia :: source -> HandlerT app IO [(FPS, Html)]

-- | Search interface to a media source.
class MediaBrowsable app source => MediaSearchable app source where

    -- | The datatype for an advanced search.
    data MSearch source :: *

    -- | Clientside javascript `function identifier(data, dom)`, which renders
    -- search results from `data` in `dom`.
    searchableJSRender  ::           Text -> source -> WidgetT app IO ()
    -- | Text search.
    searchableSearchT   ::           Text -> source -> MediaView app source
    -- | Advanced search.
    searchableSearch    :: MSearch source -> source -> MediaView app source
    -- | Advanced search form.
    searchableForm      ::                   source -> MediaAForm app (MSearch source)
