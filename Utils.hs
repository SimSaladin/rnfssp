------------------------------------------------------------------------------
-- File: Utils.hs
-- Creation Date: Aug 04 2012 [02:54:37]
-- Last Modified: Apr 03 2013 [10:37:02]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Utils where

import           Import
import           Control.Monad.Random
import           Data.Char
import           Data.Time (getCurrentTime)
import           Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import           Data.List (tail)
import           System.FilePath
import qualified System.FilePath as F
import           System.Posix (FileOffset)
import           Text.Printf (printf)
import           Yesod.Default.Config (appExtra)
import           Data.Time.Format (formatTime, FormatTime)
import           System.Locale (defaultTimeLocale)

-- * Combinators

tryMaybe :: Monad m => m a -> Maybe a -> m a
tryMaybe = flip maybe return

if' :: Bool -> a -> a -> a
if' cond th el = if cond then th else el

removeByIndex :: Int -> [a] -> [a]
removeByIndex i xs = let (ys,zs) = splitAt i xs in ys ++ tail zs

-- * Handler utils

isAdmin' :: Handler Bool
isAdmin' = liftM (maybe False $ userAdmin . entityVal) maybeAuth

-- | convienience
timeNow :: Handler UTCTime
timeNow = liftIO getCurrentTime

denyIf :: Bool -> Text -> Handler ()
denyIf True  = permissionDenied
denyIf False = const (return ())

gServeroot :: Handler Text
gServeroot = liftM (extraServeroot . appExtra . settings) getYesod

-- * 

titleRender :: [Text] -> Widget
titleRender = setTitle . toHtml . T.concat

widgetOnly :: Widget -> Handler RepHtml
widgetOnly w = widgetToPageContent w >>= \pc -> hamletToRepHtml [hamlet|^{pageBody pc}|]

-- | Convert a widget to a whole page.
widgetToRepHtml :: Yesod master => GWidget sub master () -> GHandler sub master RepHtml
widgetToRepHtml w = do pc <- widgetToPageContent w
                       hamletToRepHtml [hamlet|^{pageBody pc}|]

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

printfTime :: FormatTime t => String -> t -> String
printfTime = formatTime defaultTimeLocale

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
