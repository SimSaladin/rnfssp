{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- File: Mpd.hs
-- Creation Date: Jul 16 2012 [23:01:24]
-- Last Modified: Dec 25 2012 [18:02:07]
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
   mpdPass :: GHandler sub master Password

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

pathContents :: YesodMpd master => [Text] -> GHandler sub master [LsResult]
pathContents = execMpd . lsInfo . Path . encodeUtf8 . T.intercalate "/"

songPaths :: YesodMpd master => Text -> GHandler sub master [Text]
songPaths = liftM (map $ \(Path x) -> decodeUtf8 x)
    . execMpd . listAll . Path . encodeUtf8

execMpd :: YesodMpd master => MPD a -> GHandler sub master a
execMpd action = do
    port <- mpdPort
    host <- mpdHost
    pass <- mpdPass
    liftM handle $ liftIO $ withMPDEx host port pass action
      where
    handle res' = case res' of
        Left mpderror -> error $ show mpderror
        Right res     -> res

