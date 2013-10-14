{-# LANGUAGE RankNTypes #-}
------------------------------------------------------------------------------
-- File: Utils.hs
-- Creation Date: Aug 04 2012 [02:54:37]
-- Last Modified: Oct 11 2013 [09:02:37]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Utils where

import           Yesod
import           Text.Lucius

import           Prelude
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Random
import           Data.Char
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           System.FilePath as F
import           System.Locale (defaultTimeLocale)
import           System.Posix (FileOffset)
import           Text.Printf (printf)

-- * Combinators

if' :: Bool -> a -> a -> a
if' cond th el = if cond then th else el

map3 :: (x -> a) -> (x -> b) -> (x -> c) -> x -> (a, b, c)
map3 f g h = (,,) <$> f <*> g <*> h

tryMaybe :: Monad m => m a -> Maybe a -> m a
tryMaybe = flip maybe return

removeByIndex :: Int -> [a] -> [a]
removeByIndex i xs = let (ys,zs) = splitAt i xs in ys ++ tail zs

last' :: Int -> [a] -> [a]
last' n xs
  | l >= n    = drop (l - n) xs
  | otherwise = xs
    where l = length xs

-- * Date and time

timeNow :: MonadIO m => m UTCTime
timeNow = liftIO getCurrentTime

formatTimeZoned :: String -> UTCTime -> IO Text
formatTimeZoned format time = liftM (T.pack . f) getCurrentTimeZone
    where f = formatTime defaultTimeLocale format . flip utcToZonedTime time

formatTimeZoned' :: UTCTime -> IO Text
formatTimeZoned' = formatTimeZoned "%H:%M %d.%m.%y"

-- * Text

randomString :: Int -> IO String
randomString n = liftM (map chr) (evalRandIO $ replicateM n rnd)
  where rnd = getRandomR (48, 57)

randomText :: Int -> IO Text
randomText = liftM T.pack . randomString

-- * Files and filetypes

uniqueFilePath :: FilePath -- ^ directory
               -> FilePath -- ^ template
               -> IO FilePath
uniqueFilePath dir template = fmap ((dir </>) . appendBaseName) (randomString 10)
  where appendBaseName = replaceBaseName template . (takeBaseName template ++)

-- | File size prettified
prettyFilesize :: FileOffset -> Text
prettyFilesize off = T.pack $ toprint off
  where
    f n = printf "%.0f" (fromIntegral off / n :: Float)
    toprint x | x >= lT   = f lT ++ "T"
              | x >= lG   = f lG ++ "G"
              | x >= lM   = f lM ++ "M"
              | x >= lK   = f lK ++ "K"
              | x >= lB   = f lB ++ "B"
              | otherwise = "n/a"
        where
            [lB,lK,lM,lG,lT] = scanl (*) 1 $ replicate 4 1024

guessFiletype :: FilePath -> Text
guessFiletype fp
    | ext `elem` map ('.':) ["mkv","avi","sfv","ogm","mp4"] = "video"
    | ext `elem` map ('.':) ["flac","mid","mp3","ogg","tak","tif","tta","wav","wma","wv"] = "audio"
    | otherwise = "unknown"
    where ext = takeExtension fp

-- * Utils

denyIf :: Yesod master => Bool -> Text -> HandlerT master IO ()
denyIf True  = permissionDenied
denyIf False = const (return ())

-- | Convert a widget to a whole page.
widgetBodyToRepHtml :: Yesod master => WidgetT master IO () -> HandlerT master IO Html
widgetBodyToRepHtml w = do
    pc <- widgetToPageContent w
    giveUrlRenderer [hamlet|^{pageBody pc}|]

layoutSplitH :: WidgetT master IO () -> WidgetT master IO () -> WidgetT master IO ()
layoutSplitH w1 w2 = [whamlet|
<div .ym-grid .ym-equalize .linearize-level-1>
   <div .ym-g62 .ym-gl>^{w1}
   <div .ym-g38 .ym-gl>^{w2}
   |]

wrapMain :: WidgetT master IO () -> WidgetT master IO ()
wrapMain w = [whamlet|<main>^{w}|]

-- * Forms

data MyForm master msg res = MyForm
    { mfInfoMsg :: (msg, msg, msg)
    , mfTitle   :: msg
    , mfEnctype :: Enctype
    , mfRoute   :: Route master
    , mfFields  :: WidgetT master IO ()
    , mfActions :: WidgetT master IO ()
    , mfResult  :: FormResult res
    }

renderYaml :: Monad master => FormRender master a
renderYaml aform fragment = do
    (res, views') <- aFormToForm aform
    let views = views' []
        has (Just _) = True
        has Nothing  = False
    let widget = [whamlet|
$newline never
\#{fragment}
$forall view <- views
    <div .ym-fbox-text :fvRequired view:.required :not $ fvRequired view:.optional :has $ fvErrors view:.error>
        <label for=#{fvId view}>#{fvLabel view}
        ^{fvInput view}
        $maybe tt <- fvTooltip view
            <span .help-block>#{tt}
        $maybe err <- fvErrors view
            <span .help-block>#{err}
|]
    return (res, widget)

renderForm :: RenderMessage master msg => MyForm master msg res -> WidgetT master IO ()
renderForm = renderForm' ""

renderFormH :: RenderMessage master msg => MyForm master msg res -> WidgetT master IO ()
renderFormH = renderForm' "form-horizontal"

submitButton :: Text -> WidgetT master IO ()
submitButton x = [whamlet|<input type=submit value=#{x}>|]

submitButtonI :: RenderMessage master msg
              => msg -> WidgetT master IO ()
submitButtonI x = [whamlet|<input type=submit value=_{x}>|]

-- | Render a POST form.
renderForm' :: RenderMessage master msg
            => Text -> MyForm master msg res -> WidgetT master IO ()
            -- buttons title route res widget encType = [whamlet|
renderForm' extra settings = [whamlet|
<form .#{extra} method=post action=@{mfRoute settings} enctype=#{mfEnctype settings}>
  <legend>_{mfTitle settings}
  $case mfResult settings
      $of FormFailure [_]
        <div .alert .alert-error>_{msgError}
      $of FormFailure xs
          <div .alert .alert-error>#{length xs} _{msgErrors}
      $of FormSuccess _
          <div .alert .alert-success>_{msgSuccess}
      $of _
  ^{mfFields settings}
  <div .form-actions>^{mfActions settings}
|] where
    (msgError, msgErrors, msgSuccess) = mfInfoMsg settings

-- * Mixins

cssMyTransition :: Mixin
cssMyTransition = [luciusMixin|
-webkit-transition: opacity .4s ease-in-out;
-moz-transition: opacity .4s ease-in-out;
-ms-transition: opacity .4s ease-in-out;
-o-transition: opacity .4s ease-in-out;
transition: opacity .4s ease-in-out;
    |]

cssBorderRadius :: String -> Mixin
cssBorderRadius val = [luciusMixin|
-webkit-border-radius: #{val};
-moz-border-radius: #{val};
-ms-border-radius: #{val};
-o-border-radius: #{val};
border-radius: #{val};
    |]
