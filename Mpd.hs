{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- File: Mpd.hs
-- Creation Date: Jul 16 2012 [23:01:24]
-- Last Modified: Dec 25 2012 [00:01:51]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Mpd
   ( Mpd(..)
   , YesodMpd(..)

   , mpdFullWidget
   , mpdStatusWidget

   , pathContents
   , songPaths

   , module Network.MPD
   ) where

import Yesod
import Prelude
import Control.Monad (liftM)
import Prelude (Bool(..), ($), (.))
import Network.MPD
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

data Mpd = Mpd

class Yesod master => YesodMpd master where
   mpdPort :: GHandler sub master Port
   mpdHost :: GHandler sub master Host
   mpdPassword :: GHandler sub master Password

mkYesodSub "Mpd" [] [parseRoutes|
/status QueryR GET
|]

-- | 
getQueryR :: Yesod master => GHandler Mpd master RepHtml
getQueryR = do
   defaultLayout [whamlet|Coming soon!|]

-- | A small status widget.
mpdStatusWidget :: Yesod master => GWidget Mpd master ()
mpdStatusWidget = do
   [whamlet|(This is mpd status?)|]

-- | Full widget with all features.
mpdFullWidget :: Yesod master => GWidget Mpd master ()
mpdFullWidget = do
   [whamlet|Controlling coming soon|]

pathContents :: [Text] -> GHandler sub master [LsResult]
pathContents = execMpd . lsInfo . Path . encodeUtf8 . T.intercalate "/"

songPaths :: Text -> GHandler sub master [Text]
songPaths = liftM (map $ \(Path x) -> decodeUtf8 x) . execMpd . listAll . Path . encodeUtf8

execMpd :: MPD a -> GHandler sub master a
execMpd = liftM handle . liftIO . withMPDEx "localhost" 6600 "" where
  handle res' = case res' of
      Left mpderror -> error $ show mpderror
      Right res -> res

