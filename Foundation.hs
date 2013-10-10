{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Foundation where

import           Prelude
import           Blaze.ByteString.Builder.Char.Utf8 (fromText, fromString)
import           Control.Applicative ((<$>))
import           Control.Monad
import qualified Data.List as L
import           Data.Maybe
import           Data.Monoid    ((<>))
import           Data.Text      (Text)
import qualified Data.Text  as T
import           Data.Text.Encoding  (decodeUtf8)
import           Data.Time (getCurrentTime)
import qualified Database.Persist
import           Database.Persist.Sql (SqlPersistT)
import           System.Log.FastLogger  (Logger)
import           Text.Hamlet            (hamletFile)
import           Text.Jasmine           (minifym)
import           WaiAppStatic.Types     (StaticSettings(..), File(..), fromPiece)
import           Data.Time.Format (formatTime)
import           System.Locale (defaultTimeLocale)

import           Yesod hiding (fileName)
import           Yesod.Static
import           Yesod.Auth
import           Yesod.Auth.HashDB      (authHashDB, getAuthIdHashDB)
import           Yesod.Auth.Message     as Msg
import           Yesod.Default.Config
import           Yesod.Default.Util (addStaticContentExternal)
import           Network.HTTP.Conduit   (Manager)
import           Network.Wai (Request(..))

import           Sections.Types
import           Settings.StaticFiles
import           Settings (widgetFile, Extra (..))
import           Model
import qualified Settings
import           Settings.Development (development)
import           Chat
import           Mpd

import Utils

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

-- * Application

data App = App
    { settings      :: AppConfig DefaultEnv Extra
    , getStatic     :: Static -- ^ Settings for static file serving.
    , connPool      :: Database.Persist.PersistConfigPool Settings.PersistConf -- ^ Database connection pool.
    , httpManager   :: Manager
    , persistConfig :: Settings.PersistConf
    , appLogger     :: Logger
    , getChat       :: Chat
    , getMpd        :: Mpd
    }

-- Set up i18n messages. See the message folder.
mkMessage "App" "messages" "en"

mkYesodData "App" $(parseRoutesFile "config/routes")

type Form x = Html -> MForm (HandlerT App IO) (FormResult x, Widget)

instance Yesod App where
    approot = ApprootMaster $ appRoot . settings

    makeSessionBackend _ = fmap Just $ defaultClientSessionBackend
        (24 * 60 * 60)
        "config/client_session_key.aes"

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
        giveUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticRoot setting in Settings.hs
    urlRenderOverride y (StaticR s) =
        Just $ uncurry (joinPath y (Settings.staticRoot $ settings y)) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Just $ AuthR LoginR

    isAuthorized BlogHomeR        True = isAdmin           -- Blog
    isAuthorized MediaHomeR          _ = isValidLoggedIn'  -- Media
    isAuthorized (MediaContentR _ _) _ = isValidLoggedIn'
    isAuthorized MediaAdminR         _ = isAdmin
    isAuthorized AdminR              _ = isAdmin           -- Misc
    isAuthorized _                   _ = return Authorized

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

    makeLogger = return . appLogger

-- XXX: TODO
--    errorHandler _ = defaultLayout $ do
--        setTitle "Error"
--        navigation ""
--        [whamlet|
-- <main>
--            |]


-- How to run database actions.
instance YesodPersist App where
    type YesodPersistBackend App = SqlPersistT
    runDB = defaultRunDB persistConfig connPool
instance YesodPersistRunner App where
    getDBRunner = defaultGetDBRunner connPool

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
        lift homeIfLoggedIn
        tm <- getRouteToParent
        lift $ defaultLayout $ do
            setTitleI Msg.LoginTitle
            navigation "Login"
            master <- liftHandlerT getYesod
            wrapMain $ mapM_ (flip apLogin tm) (authPlugins master)

hashLogin :: (Route Auth -> Route App) -> Widget
hashLogin tm  = $(widgetFile "login")
  where route = tm $ PluginR "hashdb" ["login"]

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

-- * Extra

-- | Get the 'Extra' value, used to hold data from the settings.yml file.
getExtra :: Handler Extra
getExtra = fmap (appExtra . settings) getYesod

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

gServeroot :: Handler Text
gServeroot = liftM (extraServeroot . appExtra . settings) getYesod

-- * Utils

-- ** Auth

routeToLogin :: Route App
routeToLogin = AuthR LoginR

isAdmin :: Handler AuthResult
isAdmin = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just admin -> if userAdmin $ entityVal admin
            then Authorized
            else Unauthorized "You must be an admin!"
--            | admin == (Key $ Database.Persist.Store.PersistInt64 3) -> Authorized
--            | otherwise -> Unauthorized "You must be an admin"

isAdmin' :: Handler Bool
isAdmin' = liftM (maybe False $ userAdmin . entityVal) maybeAuth

isValidLoggedIn :: Handler AuthResult
isValidLoggedIn = do
    mu <- maybeAuth
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just (Entity _ uval) 
            | userValid uval -> Authorized
            | otherwise -> Unauthorized "Your user is not (yet) validated. You must be approved by an admin first"

isValidLoggedIn' :: Handler AuthResult
isValidLoggedIn' = isValidLoggedIn >>= \x -> case x of
    AuthenticationRequired -> setMessage "That section requires authentication. Please login."
    _                      -> return ()
    >> return x

homeIfLoggedIn :: Handler ()
homeIfLoggedIn = maybeAuth >>= maybe (return ())
    (const $ setMessageI Msg.NowLoggedIn >> redirect HomePageR)

denyIfAnonUnPVL :: Handler ()
denyIfAnonUnPVL = do
    ma <- maybeAuth
    req <- waiRequest
    let proxyHeader = isAllowed . decodeUtf8 <$> L.lookup "X-Real-IP" (requestHeaders req)
    if isJust ma || (fromMaybe False proxyHeader)
      then return ()
      else setMessage "Toiminto ei sallittu. Jotkin toiminnot (esim. poistotoiminnot) vaativat sisäänkirjautumisen." >> redirect HomePageR
  where
      isAllowed x
        | "127.0."       `T.isPrefixOf` x = True
        | "194.197.235." `T.isPrefixOf` x = True
        | otherwise = False

-- ** Common widgets

data MyNav = NavHome
           | NavBlog
           | NavLauta
           | NavMedia
           | NavMarket
           | NavAdmin
           | NavProfile
           | NavOther Text
           deriving (Eq)

type NavElements = [(MyNav, Widget)]

myNav :: NavElements
myNav = [ (NavHome, [whamlet|Animu|]) ]

navigation' :: MyNav -> Widget
navigation' _current = do
    undefined

navigation :: Text -> Widget
navigation active = do
    ma <- liftHandlerT maybeAuth
    boards <- liftM boards2widget $ liftHandlerT $ runDB $ selectList [] []
    mmsg <- liftHandlerT getMessage
    let es =
          [ ("Animu", Right HomePageR)
          , ("Blog",  Right BlogHomeR)
          , ("Lauta", Left (BoardHomeR, boards))
          , ("Media", Right MediaHomeR)
          , ("Market",Right MarketHomeR)
          ]

    let es' = [ ("Kemia",    "http://kemia.ssdesk.paivola.fi")
              , ("Gitlist",  "http://gitlist.ssdesk.paivola.fi")
              , ("Projects", "http://projects.ssdesk.paivola.fi")
              ] :: [ (Text, Text) ]
    [whamlet|
<header>
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
                <sup>&#8689;
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

myForm :: AppMessage -> Enctype -> Route App -> Widget -> Widget -> FormResult res -> MyForm App AppMessage res
myForm = MyForm (MsgFormOneError, MsgFormNErrors, MsgFormSuccess)

-- * Subsites

-- ** Chat

instance YesodChat master => YesodSubDispatch Chat (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesChat)

instance YesodChat App where
    data ChatMessage App = CMsg Chatmsg

    chatIdent = liftM (fmap $ userUsername . entityVal) maybeAuth

    chatCreateMsg poster content = do
        time <- liftIO getCurrentTime
        let msg = Chatmsg time poster content
        _ <- runDB $ insert msg
        return $ CMsg msg

    chatGet = liftM (map $ CMsg . entityVal) $ runDB $ selectList [] []

    chatRenderMsg (CMsg (Chatmsg time poster msg)) =
        fromString ("<p><span class=date>" <> formatTime defaultTimeLocale "%H:%M" time <> "</span> ")
            <> fromText ("<span class=poster> " <> poster <> "</span> ")
            <> fromText msg

-- ** Mpd

instance YesodMpd master => YesodSubDispatch Mpd (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesMpd)

instance YesodMpd App where
  mpdPort = return 6600
  mpdHost = return "localhost"
  mpdPass = return ""
