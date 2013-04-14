module Foundation where

import Prelude hiding (appendFile, readFile)

import           Control.Monad  (liftM)
import           Data.Maybe
import           Data.Monoid    (mappend)
import           Data.Text      (Text)
import qualified Data.Text  as T
import           Data.Text.IO   (appendFile, readFile)
import           Database.Persist.GenericSql
import qualified Database.Persist.Store
import           Network.HTTP.Conduit   (Manager)
import           System.Log.FastLogger  (Logger)
import           Text.Hamlet            (hamletFile)
import           Text.Jasmine           (minifym)
import           WaiAppStatic.Types     (StaticSettings(..), File(..), fromPiece)
import           Web.ClientSession      (getKey)
import           Yesod                  hiding (fileName)
import           Yesod.Auth
import           Yesod.Auth.HashDB      (authHashDB, getAuthIdHashDB)
import qualified Yesod.Auth.Message     as Msg
import           Yesod.Default.Config
import           Yesod.Default.Util     (addStaticContentExternal)
import           Yesod.Static

import           Settings.Development (development)
import           Settings.StaticFiles
import           Settings (widgetFile, Extra (..))
import           Model
import qualified Settings
import           Chat
import           Mpd

-- | The site argument for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { settings      :: AppConfig DefaultEnv Extra
    , getStatic     :: Static -- ^ Settings for static file serving.
    , connPool      :: Database.Persist.Store.PersistConfigPool Settings.PersistConfig -- ^ Database connection pool.
    , httpManager   :: Manager
    , persistConfig :: Settings.PersistConfig
    , appLogger     :: Logger
    , getChat       :: Chat
    , getMpd        :: Mpd
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

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend _ = do
        key <- getKey "config/client_session_key.aes"
        let timeout = 120 * 60 -- 120 minutes
        (getCachedDate, _closeDateCache) <- clientSessionDateCacher timeout
        return . Just $ clientSessionBackend2 key getCachedDate

    defaultLayout widget = do
        master <- getYesod
        pc <- widgetToPageContent $ do
            addStylesheet $ StaticR $ if development
                then yaml_core_base_css
                else yaml_core_base_min_css
            addStylesheet $ StaticR yaml_screen_typography_css
            addStylesheet $ StaticR css_fontello_css

            -- TODO: theme changer
            $(widgetFile "theme_senjougahara")
            addScript $ StaticR js_json2_js
            addScript $ StaticR js_zepto_min_js
            $(widgetFile "default-layout")
        hamletToRepHtml $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    -- Blog
    isAuthorized BlogHomeR True  = isAdmin
    -- Media
    isAuthorized MediaHomeR          _ = isValidLoggedIn'
    isAuthorized (MediaContentR _ _) _ = isValidLoggedIn'
    isAuthorized MediaAdminR         _ = isAdmin
    -- Misc
    isAuthorized AdminR        _     = isAdmin
    isAuthorized _                   _ = return Authorized

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent =
        addStaticContentExternal minifier genFileName Settings.staticDir (StaticR . flip StaticRoute [])
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs
            | development = "autogen-" ++ base64md5 lbs
            | otherwise   = base64md5 lbs
        minifier = if development then Right else minifym

    -- Place Javascript at bottom of the body tag so the rest of the page loads first
    jsLoader _ = BottomOfBody

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLog _ _source level =
        development || level == LevelWarn || level == LevelError

    getLogger = return . appLogger

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

    loginDest         _ = HomePageR
    logoutDest        _ = HomePageR
    redirectToReferer _ = True 
    onLogout            = setMessageI MsgLoggedOut

    getAuthId       = getAuthIdHashDB AuthR (Just . UniqueUser)
    authPlugins   _ = [ (authHashDB $ Just . UniqueUser) { apLogin = hashLogin } ]
    authHttpManager = httpManager

    loginHandler = do
        homeIfLoggedIn
        defaultLayout $ do
            setTitleI Msg.LoginTitle
            navigation "Login"
            tm <- lift getRouteToMaster
            master <- lift getYesod
            mapM_ (flip apLogin tm) (authPlugins master)

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
  saveMessage (from, msg) = liftIO $ appendFile chatFile $ from `mappend` " " `mappend` msg `mappend` "\n"
  getRecent = liftM (map (T.breakOn " ") . last' 5 . T.lines) $ liftIO $ readFile chatFile

instance YesodMpd App where
  mpdPort = return 6600
  mpdHost = return "localhost"
  mpdPass = return ""

setStaticSettings :: Static -> Static
setStaticSettings (Static ss) = Static $ ss {
    ssGetMimeType = \x -> fromMaybe (ssGetMimeType ss $ x) (getMT x)
  } where
  getMT file
      | endsWith ".eot"  = Just $ return "application/vnd.ms-fontobject"
      | endsWith ".woff" = Just $ return "application/x-font-woff"
      | endsWith ".ttf"  = Just $ return "application/x-font-ttf"
      | endsWith ".svg"  = Just $ return "image/svg+xml"
      | otherwise        = Nothing
    where endsWith x = x `T.isSuffixOf` (fromPiece $ fileName file)


-- * Utils

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

isValidLoggedIn :: GHandler s App AuthResult
isValidLoggedIn = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just (Entity _ uval) 
            | userValid uval -> Authorized
            | otherwise -> Unauthorized "Your user is not (yet) validated. You must be approved by an admin first"

isValidLoggedIn' :: GHandler s App AuthResult
isValidLoggedIn' = isValidLoggedIn >>= \x -> case x of
    AuthenticationRequired -> setMessage "That section requires authentication. Please login."
    _                      -> return ()
    >> return x

hashLogin :: (Route Auth -> Route App) -> GWidget sub App ()
hashLogin tm  = $(widgetFile "login")
  where route = tm $ PluginR "hashdb" ["login"]

homeIfLoggedIn :: GHandler sub App ()
homeIfLoggedIn = maybeAuth >>= maybe (return ())
    (const $ setMessageI Msg.NowLoggedIn >> redirect HomePageR)

last' :: Int -> [a] -> [a]
last' n xs
  | l >= n    = drop (l - n) xs
  | otherwise = xs
    where l = length xs

routeToLogin :: Route App
routeToLogin = AuthR LoginR

navigation :: Text -> GWidget sub App ()
navigation active = do
    ma <- lift maybeAuth
    boards <- liftM boards2widget $ lift $ runDB $ selectList [] []
    mmsg <- lift getMessage
    let es =
          [ ("SS", Right HomePageR)
          , ("Blog" :: Text, Right BlogHomeR)
          , ("Lauta", Left (BoardHomeR, boards))
          , ("Media", Right MediaHomeR)
          ]

    let es' = [ ("Kemia",    "http://kemia.ssdesk.paivola.fi")
              , ("Gitlist",  "http://gitlist.ssdesk.paivola.fi")
              , ("Projects", "http://projects.ssdesk.paivola.fi")
              ] :: [ (Text, Text) ]
    [whamlet|
<header #main-header>
    <nav .ym-wrapper>
        <ul>
          $forall (topic, e) <- es
            $with isactive <- topic == active
              <li :isactive:.active>
                $case e
                  $of Left stuff
                    $with (route, w) <- stuff
                        <a href=@{route}>#{topic}
                        ^{w}
                  $of Right route
                    <a href=@{route}>#{topic}
          $forall (topic, href) <- es'
            <li>
              <a href="#{href}">
                <i>#{topic}
        <ul .pull-right>
          $maybe authent <- ma
            $if userAdmin $ entityVal authent
              $with isactive <- active == "Admin"
                <li :isactive:.active>
                  <a href=@{AdminR}>Admin
            <li>
              $with isactive <- active == "Profile"
                <a href=@{ProfileR} :isactive:.active>#{userUsername $ entityVal authent}
            <li .divider-vertical>
            <li>
              <a href=@{AuthR LogoutR}>_{MsgLogout}
          $nothing
            $with isactive <- active == "Register"
              <li :isactive:.active>
                <a href=@{RegisterR}>_{MsgRegister}
            $with isactive <- active == "Login"
              <li :isactive:.active>
                <a href=@{AuthR LoginR}>_{MsgLogin}
$maybe msg <- mmsg
 <p #message .box .info>#{msg}
  |] where boards2widget boards = [whamlet|$newline never
$if null boards
$else
   <ul>
      $forall Entity _ val <- boards
         <li>
            <a href=@{BoardR (boardName val)}>
               <b>/#{boardName val}/ #
               <i>#{boardDescription val}
|]
