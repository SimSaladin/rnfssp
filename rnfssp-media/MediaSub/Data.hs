{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GADTs              #-}

-- | Subsite routes, database entries and miscellaneus types.
module MediaSub.Data where

import ClassyPrelude
import Data.Conduit
import Data.Default
import Data.Time.Clock (NominalDiffTime)
import Database.Persist.Sql (SqlBackend)
import Language.Haskell.TH.Syntax
import Text.Coffee
import Text.Hamlet
import Yesod
import Yesod.Auth
import Yesod.Default.Util
import qualified Data.Map as Map

-- | The media subsite. Default instance uses http://localhost:3000 for
-- unsecure serve root.
data MediaSub = MediaSub
              { mediaMaxTempUrlAlive :: NominalDiffTime -- ^ How long temporary urls are valid.
              , mediaTempServeRoot :: Text -- ^ The root from which to serve temporary files (in .m3u playlists).
                                          -- This should be e.g. approot, but note that most players won't play
                                          -- over https.
              }

instance Default MediaSub where
    def = MediaSub (24 * 60 * 60) "http://localhost:3000/"

-- | Configuration interface for MediaSub. See "MediaSub.Standalone" for
-- an example.
class ( YesodAuthPersist m
      , PersistEntityBackend (KeyEntity (AuthId m)) ~ SqlBackend
      , PersistQuery (YesodPersistBackend m (HandlerT m IO))
      , PersistUnique (YesodPersistBackend m (HandlerT m IO))
      , MonadTrans (YesodPersistBackend m)
      ) => YesodMediaSub m where

    -- | User identifier used for playlists and logging.
    mediaIdent :: HandlerT m IO Text

    mediaSections :: Map SectionId (Section m)

    -- | Map section id's to sections.
    mediaGetSection :: SectionId -> HandlerT m IO (Section m)
    mediaGetSection _ = error "mediaGetSections is not supposed to be used"

    -- | Get all sections.
    mediaGetSections :: HandlerT m IO [(SectionId, Section m)]
    mediaGetSections = error "mediaGetSections is not supposed to be used"

buildSections :: [(SectionId, SectionId -> Section m)] -> Map SectionId (Section m)
buildSections = Map.fromList . map (\(sid, f) -> (sid, f sid))

-- * Types

-- XXX: move these to a MediaSub.Types?

-- | A section thingy. See MediaSub.Standalone for usage example.
data Section m = Section
   { secRealFile      :: FPS  -> SubHandler m FilePath -- ^ (Absolute) path to the file for serving/downloads
   , secGetContents   :: FPS  -> SubHandler m TypedContent -- ^ Browsing at given path
   , secGetPlayables  :: FPS  -> Source (HandlerT m IO) Text -- ^ File(path)s only of given path
   , secSearchWild    :: Text -> Source (HandlerT m IO) SearchResult
   }

defaultSection :: Section m
defaultSection = Section
               { secRealFile = \_ -> invalidArgs ["secRealFile not implemented for this section" ]
               , secGetContents = \_ -> invalidArgs ["secGetContents not implemented"]
               , secGetPlayables = \_ -> invalidArgs ["secGetPlayables not implemented"]
               , secSearchWild = \_ -> invalidArgs ["secSearchWild not implented"]
               }

-- | Identifier of a section.
type SectionId = Text

-- | A result from running a search
data SearchResult = SearchResult
                  { searchResultPath :: FPS }

type SubHandler m a = HandlerT MediaSub (HandlerT m IO) a
type Handler      a = forall m. YesodMediaSub m => SubHandler m a
type Widget         = WidgetT MediaSub IO ()

-- I think it should be WidgetT MediaSub ?
type Form m x  = Html -> MForm (HandlerT m IO) (FormResult x, WidgetT m IO ())

-- | A path into a section.
type FPS = [Text]

data ServeType = ServeTemp
               | ServeAuto
               | ServeForceDownload
               deriving (Show, Read, Eq)
instance PathPiece ServeType where
  toPathPiece ServeTemp          = "temp"
  toPathPiece ServeAuto          = "auto"
  toPathPiece ServeForceDownload = "force"
  fromPathPiece "temp"  = Just ServeTemp
  fromPathPiece "auto"  = Just ServeAuto
  fromPathPiece "force" = Just ServeForceDownload
  fromPathPiece       _ = Nothing

type PlaylistElem = (SectionId, Text)

-- * Persistent

-- Database fields
share [mkPersist sqlSettings, mkMigrate "migrateMedia"] [persistUpperCase|
Playlist
   title        Text
   owner        Text
   elems        [PlaylistElem]
   created      UTCTime
   modified     UTCTime
   UniquePlaylist owner title
Filenode
   area         Text
   parent       FilenodeId Maybe
   isdir        Bool
   path         Text
   size         Text
   modTime      UTCTime
   details      Text Maybe
   UniqueFilenode area path
DNode
   area         Text
   parent       DNodeId Maybe
   path         Text
   UniqueDNode area path
FNode
   area         Text
   parent       DNodeId Maybe
   path         Text
   details      Text Maybe
   UniqueFNode area path
LogDownload
   time         UTCTime
   user         Text
   section      Text
   filepath     Text
DlTemp
   time         UTCTime
   temp         Text
   section      Text
   path         Text
   UniqueDlTemp temp
RecentlyAdded
   date         UTCTime
   section      Text
   path         Text
   desc         Html
|]

instance ToJSON Playlist where
    toJSON (Playlist title owner elems create modified) = object
        [ "title"    .= title
        , "owner"    .= owner
        , "elems"    .= elems
        , "created"  .= create
        , "modified" .= modified
        ]

instance FromJSON Playlist where
  parseJSON (Object v) = Playlist <$> v .: "title"
                                  <*> v .: "owner"
                                  <*> v .: "elems"
                                  <*> v .: "created"
                                  <*> v .: "modified"
  parseJSON          _ = undefined

-- * Routes

-- 
mkYesodSubData "MediaSub" [parseRoutes|
/                                MediaHomeR      GET
/admin                           MediaAdminR     GET POST
/browse/#Text/*FPS               MediaContentR   GET
/search                          MediaSearchAllR GET
/search/#Text                    MediaSearchR    GET
/playlist/#Text                  PlaylistR       GET POST
/files/#ServeType/#Text/*FPS     MediaServeR     GET
|]

-- * Misc
-- XXX: Move these to .Import

fpsToFilePath :: FPS -> FilePath
fpsToFilePath = concatMap fpFromText

fpsToText :: FPS -> Text
fpsToText = intercalate "/" 

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    , wfsLanguages = \hset -> defaultTemplateLanguages hset ++ [TemplateLanguage True "coffee" coffeeFile coffeeFileReload]
    }

widgetFile :: String -> Q Exp
widgetFile = -- (if development then widgetFileReload
        widgetFileNoReload widgetFileSettings
