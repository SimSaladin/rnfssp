{-# LANGUAGE FlexibleInstances #-}
------------------------------------------------------------------------------
-- File: Mpd.hs
-- Creation Date: Jul 16 2012 [23:01:24]
-- Last Modified: Jul 18 2012 [19:00:19]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Mpd
   ( Mpd(..)
   , YesodMpd(..)
   , mpdFullWidget
   , mpdStatusWidget
   ) where

import Yesod
import Prelude (Bool(..), ($), (.))
import Network.MPD

data Mpd = Mpd

class Yesod master => YesodMpd master where
   mpdPort :: GHandler sub master Port
   mpdHost :: GHandler sub master Host
   mpdPassword :: GHandler sub master Password

mkYesodSub "Mpd" [] [parseRoutes|
/status QueryR GET
|]

getQueryR :: Yesod master => GHandler Mpd master RepHtml
getQueryR = do
   defaultLayout [whamlet|Coming soon!|]

mpdStatusWidget :: Yesod master => GWidget Mpd master ()
mpdStatusWidget = do
   [whamlet|(This is mpd status?)|]

mpdFullWidget :: Yesod master => GWidget Mpd master ()
mpdFullWidget = do
   [whamlet|Controlling coming soon|]
