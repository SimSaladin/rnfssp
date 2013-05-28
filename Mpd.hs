{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- File: Mpd.hs
-- Creation Date: Jul 16 2012 [23:01:24]
-- Last Modified: May 28 2013 [14:27:55]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Mpd where

import Yesod
import Prelude
import Control.Monad (liftM)
import Network.MPD
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

data Mpd = Mpd

class Yesod master => YesodMpd master where
   mpdPort :: HandlerT master IO Port
   mpdHost :: HandlerT master IO Host
   mpdPass :: HandlerT master IO Password

mkYesodSubData "Mpd" [parseRoutes|
/status QueryR GET
|]

-- | 
getQueryR :: Yesod master => HandlerT Mpd (HandlerT master IO) RepHtml
getQueryR = do
    lift $ defaultLayout [whamlet|Coming soon!|]

-- | A small status widget.
mpdStatusWidget :: WidgetT Mpd (HandlerT master IO) ()
mpdStatusWidget = do
    [whamlet|(This is mpd status?)|]

-- | Full widget with all features.
mpdFullWidget :: WidgetT Mpd (HandlerT master IO) ()
mpdFullWidget = do
    [whamlet|Controlling coming soon|]

pathContents :: YesodMpd master
             => [Text] -> HandlerT master IO [LsResult]
pathContents = execMpd . lsInfo . Path . encodeUtf8 . T.intercalate "/"

songPaths :: YesodMpd master
          => [Text] -> HandlerT master IO [Text]
songPaths = liftM (f . concat) . mapM (execMpd . listAll . Path . encodeUtf8)
        where f = map $ \(Path x) -> decodeUtf8 x

execMpd :: YesodMpd master
        => MPD a -> HandlerT master IO a
execMpd action = do
    port <- mpdPort
    host <- mpdHost
    pass <- mpdPass
    liftM handle $ liftIO $ withMPDEx host port pass action
      where
    handle res' = case res' of
        Left mpderror -> error $ show mpderror
        Right res     -> res

