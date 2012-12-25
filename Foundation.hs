module Foundation where

import Prelude hiding (appendFile, readFile)
import Control.Monad (liftM)
import Yesod
import Yesod.Static
import Yesod.Auth
import Yesod.Auth.HashDB (authHashDB, getAuthIdHashDB)
import Yesod.Default.Config
import Yesod.Default.Util (addStaticContentExternal)
import Network.HTTP.Conduit (Manager)
import qualified Settings
import Settings.Development (development)
import qualified Database.Persist.Store
import Settings.StaticFiles
import Database.Persist.GenericSql
import Settings (widgetFile, Extra (..))
import Model
import Text.Jasmine (minifym)
import Web.ClientSession (getKey)
import Text.Hamlet (hamletFile)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.IO (appendFile, readFile)
import Data.Monoid (mappend)

import Chat
import Mpd

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings :: AppConfig DefaultEnv Extra
    , getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager :: Manager
    , persistConfig :: Settings.PersistConfig
    , getChat :: Chat
    , getMpd :: Mpd
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/handler
--
-- This function does three things:
--
-- * Creates the route datatype AppRoute. Every valid URL in your
--   application can be represented as a value of this type.
-- * Creates the associated type:
--       type instance Route App = AppRoute
-- * Creates the value resourcesApp which contains information on the
--   resources declared below. This is used in Handler.hs by the call to
--   mkYesodDispatch
--
-- What this function does *not* do is create a YesodSite instance for
-- App. Creating that instance requires all of the handler functions
-- for our application to be in scope. However, the handler functions
-- usually require access to the AppRoute datatype. Therefore, we
-- split these actions into two functions and place them in separate files.
mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm App App (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        return . Just $ clientSessionBackend key 2160

    defaultLayout widget = do
        master <- getYesod
        mmsg <- getMessage
        boards <- runDB $ selectList ([] :: [Filter Board]) []
        ma <- maybeAuth

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
            $(widgetFile "normalize")
            addStylesheet $ StaticR css_bootstrap_css
            let cwidget = chatWidget ChatR
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized AdminR        _     = isAdmin
    isAuthorized MediaAdminR   _     = isAdmin
    isAuthorized BlogOverviewR True  = isAdmin
    isAuthorized MediaHomeR    False = return Authorized
    isAuthorized (MediaContentR _ _) _ = isValidLoggedIn
    isAuthorized _             _     = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent = addStaticContentExternal minifym base64md5 Settings.staticDir (StaticR . flip StaticRoute [])

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

isAdmin :: GHandler s App AuthResult
isAdmin = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just admin -> if userAdmin $ entityVal admin
            then Authorized
            else Unauthorized "You must be an admin!"
--            | admin == (Key $ Database.Persist.Store.PersistInt64 3) -> Authorized
--            | otherwise -> Unauthorized "You must be an admin"

--isValidLoggedIn :: (PersistStore (YesodPersistBackend m) (GHandler s m),
--        YesodPersist m, YesodAuth m, AuthId m ~ Key (YesodPersistBackend m)
--        (UserGeneric (YesodPersistBackend m))) => GHandler s m AuthResult
isValidLoggedIn :: GHandler s App AuthResult
isValidLoggedIn = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just (Entity _ uval) 
            | userValid uval -> Authorized
            | otherwise -> Unauthorized "Your user is not (yet) validated. You must be approved by an admin first"

-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersist
    runDB f = do
        master <- getYesod
        Database.Persist.Store.runPool
            (persistConfig master)
            f
            (connPool master)

instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest _ = BlogOverviewR
    -- Where to send a user after logout
    logoutDest _ = BlogOverviewR

    getAuthId = getAuthIdHashDB AuthR (Just . UniqueUser)

    authPlugins _ = [authHashDB (Just . UniqueUser)]

    authHttpManager = httpManager

    loginHandler = defaultLayout $ do
        setTitle "Login"
        $(widgetFile "login")

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

-- Note: previous versions of the scaffolding included a deliver function to
-- send emails. Unfortunately, there are too many different options for us to
-- give a reasonable default. Instead, the information is available on the
-- wiki:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email

chatFile :: FilePath
chatFile = "chatmessages.txt"

instance YesodChat App where
  getUserName = liftM (userUsername . entityVal) requireAuth
  isLoggedIn  = isValidLoggedIn >>= \r -> return $ case r of
      Authorized -> True
      _          -> False
  saveMessage (from, msg) = liftIO $ appendFile chatFile $ from `mappend` "," `mappend` msg
  getRecent = liftM (map (T.breakOn ",") . last' 5 . T.lines) $ liftIO $ readFile chatFile

instance YesodMpd App where
  mpdPort = return 6600
  mpdHost = return "localhost"
  mpdPass = return ""

last' :: Int -> [a] -> [a]
last' n xs
  | l >= n    = drop (l - n) xs
  | otherwise = xs
    where l = length xs

routeToLogin :: Route App
routeToLogin = AuthR LoginR

