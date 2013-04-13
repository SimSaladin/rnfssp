-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Foundation.hs file.
module Settings where

import Prelude
import Control.Monad (mzero)
import Text.Shakespeare.Text (st)
import Language.Haskell.TH.Syntax
import Database.Persist.Postgresql (PostgresConf)
import Yesod.Default.Config
import Yesod.Default.Util
import Data.Text (Text)
import Data.Yaml
import Control.Applicative
import Settings.Development
import Data.Default (def)
import Text.Hamlet
import Text.Coffee
import Data.Map (Map)

-- | Which Persistent backend this site is using.
type PersistConfig = PostgresConf

-- Static setting below. Changing these requires a recompile

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticDir :: FilePath
staticDir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Foundation.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Foundation.hs
staticRoot :: AppConfig DefaultEnv x -> Text
staticRoot conf = [st|#{appRoot conf}/static|]

-- | Settings for 'widgetFile', such as which template languages to support and
-- default Hamlet settings.
widgetFileSettings :: WidgetFileSettings
widgetFileSettings = def
    { wfsHamletSettings = defaultHamletSettings
        { hamletNewlines = AlwaysNewlines
        }
    , wfsLanguages = \hset -> defaultTemplateLanguages hset ++ [TemplateLanguage True "coffee" coffeeFile coffeeFileReload]
    }

-- The rest of this file contains settings which rarely need changing by a
-- user.

widgetFile :: String -> Q Exp
widgetFile = (if development then widgetFileReload
                             else widgetFileNoReload)
              widgetFileSettings

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

data Extra = Extra
    { extraCopyright :: Text
    , extraApproot   :: Text
    , extraServeroot :: Text
    , extraAnalytics :: Maybe Text -- ^ Google Analytics
    , extraDirDyn    :: FilePath
    , extraSections  :: Map Text MediaConf
    } deriving Show

parseExtra :: DefaultEnv -> Object -> Parser Extra
parseExtra _ o = Extra
    <$> o .:  "copyright"
    <*> o .:  "approot"
    <*> o .:  "serveroot"
    <*> o .:? "analytics"
    <*> o .: "dyndir"
    <*> o .: "mediasections"
