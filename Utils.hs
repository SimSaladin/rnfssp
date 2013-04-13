------------------------------------------------------------------------------
-- File: Utils.hs
-- Creation Date: Aug 04 2012 [02:54:37]
-- Last Modified: Apr 13 2013 [14:57:12]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Utils where

import           Control.Monad.Random
import           Data.Char
import           Data.List (tail)
import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, FormatTime)
import           Import
import           System.FilePath as F
import           System.Locale (defaultTimeLocale)
import           System.Posix (FileOffset)
import           Text.Printf (printf)
import           Yesod.Default.Config (appExtra)

-- * Combinators

tryMaybe :: Monad m => m a -> Maybe a -> m a
tryMaybe = flip maybe return

if' :: Bool -> a -> a -> a
if' cond th el = if cond then th else el

removeByIndex :: Int -> [a] -> [a]
removeByIndex i xs = let (ys,zs) = splitAt i xs in ys ++ tail zs


-- * Time utils

-- | convienience
timeNow :: Handler UTCTime
timeNow = liftIO getCurrentTime

printfTime :: FormatTime t => String -> t -> String
printfTime = formatTime defaultTimeLocale


-- * Auth

isAdmin' :: Handler Bool
isAdmin' = liftM (maybe False $ userAdmin . entityVal) maybeAuth

denyIf :: Yesod master => Bool -> Text -> GHandler sub master ()
denyIf True  = permissionDenied
denyIf False = const (return ())

gServeroot :: Handler Text
gServeroot = liftM (extraServeroot . appExtra . settings) getYesod


-- * Filepath utils

uniqueFilePath :: FilePath -- ^ directory
               -> FilePath -- ^ template
               -> IO FilePath
uniqueFilePath dir template = fmap ((dir </>) . appendBaseName) (randomString 10)
  where appendBaseName = replaceBaseName template . (takeBaseName template ++)

randomString :: Int -> IO String
randomString n = liftM (map chr) (evalRandIO $ replicateM n rnd)
  where rnd = getRandomR (48, 57)

randomText :: Int -> IO Text
randomText n = liftM (T.pack . map chr) (evalRandIO $ replicateM n rnd)
  where rnd = getRandomR (65, 90)

toPath :: [Text] -> Text
toPath = T.pack . F.joinPath . map T.unpack

-- | Split text to filepath pieces
splitPath' :: Text -> [Text]
splitPath' = map T.pack . splitPath . T.unpack

takeDirectory' :: Text -> Text
takeDirectory' = T.dropWhileEnd (=='/') . fst . T.breakOnEnd "/"


-- * Text rendering

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
    | ext `elem` (map ('.':) ["mkv","avi","sfv","ogm","mp4"]) = "video"
    | ext `elem` (map ('.':) ["flac","mid","mp3","ogg","tak","tif","tta","wav","wma","wv"]) = "audio"
    | otherwise = "unknown"
    where ext = takeExtension fp


-- * Widgets

-- | Convert a widget to a whole page.
widgetBodyToRepHtml :: Yesod master => GWidget sub master () -> GHandler sub master RepHtml
widgetBodyToRepHtml w = do
    pc <- widgetToPageContent w
    hamletToRepHtml [hamlet|^{pageBody pc}|]

layoutSplitH :: GWidget sub master () -> GWidget sub master () -> GWidget sub master ()
layoutSplitH w1 w2 = [whamlet|
<div .ym-grid .ym-equalize .linearize-level-1>
   <div .ym-g62 .ym-gl>^{w1}
   <div .ym-g38 .ym-gl>^{w2}
   |]

-- * Forms

renderYaml :: FormRender sub master a
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

-- XXX: i18n the names

submitButton :: Text -> Widget
submitButton x = [whamlet|<input type=submit value=#{x}>|]

submitButtonI :: RenderMessage App msg => msg -> Widget
submitButtonI x = [whamlet|<input type=submit value=_{x}>|]

replyButton :: Widget
replyButton = [whamlet|<input type=submit value=Reply>|]

renderForm :: RenderMessage App msg
           => GWidget sub App ()  -- ^ Widget in the submit area.
           -> msg                 -- ^ Form title
           -> Route App           -- ^ Form action.
           -> FormResult a
           -> GWidget sub App ()  -- ^ Fields.
           -> Enctype
           -> GWidget sub App ()
renderForm = renderForm' ""

renderFormH :: RenderMessage App msg
            => GWidget sub App () -- ^ Widget in the submit area.
            -> msg                -- ^ Form title
            -> Route App          -- ^ Form action.
            -> FormResult a
            -> GWidget sub App () -- ^ Fields.
            -> Enctype
            -> GWidget sub App ()
renderFormH = renderForm' "form-horizontal"

-- | Render a POST form.
renderForm' :: RenderMessage App msg
           => Text                -- ^ Extra class for <form>.
           -> GWidget sub App ()  -- ^ Widget in the submit area.
           -> msg                 -- ^ Form title
           -> Route App           -- ^ Form action.
           -> FormResult a
           -> GWidget sub App ()  -- ^ Fields.
           -> Enctype
           -> GWidget sub App ()
renderForm' extra buttons title route res widget encType = [whamlet|
<form .#{extra} method=post action=@{route} enctype=#{encType}>
  <legend>_{title}
  $case res
      $of FormFailure [_]
        <div .alert .alert-error>_{MsgFormOneError}
      $of FormFailure xs
          <div .alert .alert-error>#{length xs} _{MsgFormNErrors}
      $of FormSuccess _
          <div .alert .alert-success>_{MsgFormSuccess}
      $of _
  ^{widget}
  <div .form-actions>
    ^{buttons}
|]

