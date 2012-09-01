------------------------------------------------------------------------------
-- File: Utils.hs
-- Creation Date: Aug 04 2012 [02:54:37]
-- Last Modified: Sep 01 2012 [05:10:45]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Utils where

import           Import
import           Control.Monad
import           Control.Monad.Random
import           Data.Char
import           Data.Time (getCurrentTime)
import           Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import qualified Data.Map as Map
import           System.FilePath
import           System.Posix (FileOffset)
import           Text.Printf (printf)
import           Yesod.Default.Config (appExtra)

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
randomString n = liftM (map chr) (evalRandIO $ sequence $ replicate n rnd)
  where rnd = getRandomR (48, 57)

mapField :: Field sub master (Map.Map Text a)
mapField = Field
  { fieldParse = \rawVals -> case rawVals of
      xs | length xs < 4 -> return $ Left "Must have at least two options"
         | otherwise -> do
            let paired = pairs xs
            return $ Right $ Just Map.empty

  , fieldView = \idAttr nameAttr _ eResult isReq -> do
    j_add <- lift newIdent
    toWidget [julius|
function #{j_add}{
}
    |]
    [whamlet|
<input id=#{idAttr}-1-factor name="#{nameAttr}-factor" type=text required>
<input id=#{idAttr}-1 name=#{nameAttr} type=text :isReq:required>
<br>
<input id=#{idAttr}-2-factor name="#{nameAttr}-factor" type=text required>
<input id=#{idAttr}-2 name=#{nameAttr} type=text :isReq:required>
<br>
<button onclick="#{j_add}()">Add new option
    |]
  }

pairs :: [a] -> [ (a,a) ]
pairs []       = []
pairs (x:[])   = [(x,x)]
pairs (x:y:zs) = (x,y) : pairs zs

passwordConfirmField :: Field sub master Text
passwordConfirmField = Field
    { fieldParse = \rawVals ->
        case rawVals of
            [a, b]
                | T.length a < 4 -> return $ Left "Password must be at least 4 characters"
                | a == b -> return $ Right $ Just a
                | otherwise -> return $ Left "Passwords don't match"
            [] -> return $ Right Nothing
            _ -> return $ Left "You must enter two values"
    , fieldView = \idAttr nameAttr _ eResult isReq -> [whamlet|
<div.control-group>
  <div.control-label for=#{idAttr}> Password
  <div.controls>
    <input id=#{idAttr} name=#{nameAttr} type=password :isReq:required>
<div.control-group>
  <div.control-label for=#{idAttr}-confirm> Confirm password
  <div.controls>
    <input id=#{idAttr}-confirm name=#{nameAttr} type=password>
|]
    }
