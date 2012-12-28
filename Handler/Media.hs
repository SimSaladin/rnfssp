{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, DoAndIfThenElse #-}
module Handler.Media
    ( getMediaHomeR
    , getMediaContentR
    , getMediaServeR
    , postMediaAdminR
    , adminWidget
    ) where

import           Utils
import           Import
import           Configs
import           JSBrowser (browser)
import           Handler.Playlists (userPlaylistWidget)
import           Data.List (head, tail)
import qualified Data.Text as T
import           Data.Time.Clock (diffUTCTime, NominalDiffTime)

-- | Maximum time a file can be accessed via a temporary url.
maxTempUrlAlive :: NominalDiffTime
maxTempUrlAlive = 60 * 60 * 24

browsable :: [(Text, Text)]
browsable = [ ("anime", "film"), ("music", "music")]


-- * General content

getMediaHomeR :: Handler RepHtml
getMediaHomeR = do
  mauth <- maybeAuth
  defaultLayout $ do
    setTitle "Media"
    $(widgetFile "media-home")
  where nav = navigationWidget ""

getMediaContentR :: Text -> [Text] -> Handler RepHtml
getMediaContentR section fps = do
  mauth <- maybeAuth
  bare <- lookupGetParam "bare"
  case bare of
      Just _ -> widgetToRepHtml $ sectionWidget section fps
      Nothing -> defaultLayout $ do
          setTitle "Media"
          $(widgetFile "media-content")
  where nav = navigationWidget section

navigationWidget :: Text -> Widget
navigationWidget current = [whamlet|
<ul .nav .nav-pills>
  $forall (name, icon) <- browsable
    $if current == name
      <li .active>
        <a href=@{getRoute name}>
          <i .icon-white .icon-#{icon}>
          &nbsp;#{name}
    $else
      <li>
        <a href=@{getRoute name}>
          <i .icon-white .icon-#{icon}>
          &nbsp;#{name}
  |] where getRoute = flip MediaContentR []

restrictedWidget :: Widget
restrictedWidget = [whamlet|
<div.hero-unit.center-box.small-box>
  <h1>Access restricted
  <p>
    You must be explicitly granted access to this part.<br/>
    Please, #
    <a.btn.btn-primary href="@{routeToLogin}">Login
    &nbsp;or #
    <a.btn.btn-info href="@{RegisterR}">Register
    .
|]

-- | Generate content based on section and path.
sectionWidget :: Text -> [Text] -> Widget
sectionWidget s fps = onSec s (`content` fps)


-- * Playing / Downloading files

-- | Downloading files
getMediaServeR :: Text   -- ^ kind of download
               -> Text   -- ^ file section
               -> [Text] -- ^ file path
               -> Handler RepJson
getMediaServeR kind section path
  | null path       = invalidArgs ["Invalid file."]
  | kind == "temp"  = solveTemp >>= send ""
  | kind == "auto"  = solvePathWithAuth >>= send ""
  | kind == "force" = solvePathWithAuth >>= send "application/force-download"
  | otherwise       = invalidArgs ["Invalid or unsupported download type."]
    where
  send ct fp = setHeader "Accept-Ranges" "bytes" >> sendFile ct fp

  solveTemp  = do
    Entity _ (DlTemp time _ target) <- runDB $ getBy404 $ UniqueDlTemp $ head path
    now <- timeNow
    denyIf (diffUTCTime now time > maxTempUrlAlive) "File not available."
    denyIf (target /= toPath (tail path)          ) "Malformed url."
    toFSPath section $ T.unpack target

  solvePathWithAuth = do
    uid <- requireAuthId
    fp  <- onSec section (flip filepath $ toPath path)
    t   <- timeNow
    _   <- runDB $ insert $ LogDownload uid t fp -- TODO: use a log file
    return fp


-- * Adminspace

-- | A widget for media administration functionality, to be embebbed in the
--   centralised admin page.
adminWidget :: Widget
adminWidget = do
  ((result, widget), encType) <- lift $ runFormPost adminForm
  [whamlet|
<h1>Media
<div>
  <form.form-horizontal method=post action=@{MediaAdminR} enctype=#{encType}>
    <fieldset>
      <legend>Media actions
      $case result
        $of FormFailure reasons
          $forall reason <- reasons
            <div .alert .alert-error>#{reason}
        $of _
      ^{widget}
      <div .form-actions>
        <input .btn .primary type=submit value="Execute actions">
  |]

-- | Admin operations in Media.
postMediaAdminR :: Handler RepHtml
postMediaAdminR = do
  ((result, _), _) <- runFormPost adminForm
  case result of
    FormSuccess (True,_) -> onSec "anime" updateIndex -- TODO update all
    FormSuccess _        -> setMessage "No actions."
    _ -> setMessage "Form failed!"
  redirect AdminR

adminForm :: Form (Bool, Bool)
adminForm = renderBootstrap $ (,)
   <$> areq boolField "Update every index" (Just False)
   <*> areq boolField "Not used" (Just False)
