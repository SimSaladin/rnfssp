------------------------------------------------------------------------------
-- File: Utils.hs
-- Creation Date: Aug 04 2012 [02:54:37]
-- Last Modified: Dec 26 2012 [15:38:35]
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

isAdmin' :: Handler Bool
isAdmin' = maybeAuth >>= \ma -> return $ case ma of
    Nothing -> False
    Just ent -> userAdmin $ entityVal ent

titleRender :: [Text] -> Widget
titleRender = setTitle . toHtml . T.concat

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
            [lB,lK,lM,lG,lT] = scanl (*) 1 $ take 4 $ repeat 1024

guessFiletype :: FilePath -> Text
guessFiletype fp = if ext `elem` (map ('.':) ["mkv","avi","sfv","ogm","mp4"])
   then "video"
   else if ext `elem` (map ('.':) ["flac","mid","mp3","ogg","tak","tif","tta","wav","wma","wv"])
      then "audio"
      else "unknown"
   where ext = takeExtension fp

-- | Get real filepath for @which@ master directory.
gdir :: Text -> Handler FilePath
gdir which = do
   master <- getYesod
   let set = appExtra $ settings master
      in case which of
            "anime" -> return $ extraDirAnime set
            "music" -> return $ extraDirMusic set
            _ -> invalidArgs ["no master directory for: " `T.append` which]

-- | convert section+path to an actual file.
toFSPath :: Text -> FilePath -> Handler FilePath
toFSPath section path = liftM (</> path) $ gdir section

gServeroot :: Handler Text
gServeroot = getYesod >>= return . extraServeroot . appExtra . settings

widgetOnly :: Widget -> Handler RepHtml
widgetOnly w = widgetToPageContent w >>= \pc -> hamletToRepHtml [hamlet|^{pageBody pc}|]

-- | convienience
timeNow :: Handler UTCTime
timeNow = liftIO getCurrentTime

tryMaybe :: Monad m => m a -> Maybe a -> m a
tryMaybe this unlessJust = case unlessJust of
    Just a -> return a
    Nothing -> this

if' :: Bool -> a -> a -> a
if' cond th el = if cond then th else el

denyIf :: Bool -> Text -> Handler ()
denyIf True  = permissionDenied
denyIf False = const (return ())

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

printfTime :: FormatTime t => String -> t -> String
printfTime = formatTime defaultTimeLocale

-- | Convert a widget to a whole page.
widgetToRepHtml :: Yesod master => GWidget sub master () -> GHandler sub master RepHtml
widgetToRepHtml w = do pc <- widgetToPageContent w
                       hamletToRepHtml [hamlet|^{pageBody pc}|]

removeByIndex :: Int -> [a] -> [a]
removeByIndex i xs = let (ys,zs) = splitAt i xs in ys ++ tail zs
