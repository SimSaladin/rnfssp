------------------------------------------------------------------------------
-- File:          MPDSection.hs
-- Creation Date: Dec 24 2012 [00:26:24]
-- Last Modified: Apr 13 2013 [18:26:17]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections.Music (MPDSec, mkMPDSec) where

import           Sections
import           Import
import           Utils
import           JSBrowser
import qualified Mpd as MPD
import           Data.List (last)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           System.FilePath ((</>))

data MPDSec = MPDSec { sName  :: Text
                     , sRoot  :: FilePath
                     , sRoute :: [Text] -> Route App
                     }

mkMPDSec :: Section -> MediaConf -> MPDSec
mkMPDSec section mc = MPDSec section (mcPath mc) (MediaContentR section)

instance MSection MPDSec where
  sFind _         = MPD.songPaths
  sWContent       = musicContent
  sWSearch        = undefined -- TODO
  sFilePath MPDSec{sRoot = root} = return . (</>) root . T.unpack
  sUpdateIndex _  = return () -- TODO: mpd update?

musicContent :: MPDSec -> [Text] -> Widget
musicContent sec@MPDSec{sRoute = route} fps = do
  contents <- lift $ MPD.pathContents fps
  case contents of
    [MPD.LsSong song] -> songSingle sec fps song
    [MPD.LsPlaylist (MPD.PlaylistName bs)] -> undefined
    _ -> let sl = SimpleListingSettings
                 { slSect       = sName sec
                 , slCurrent    = fps
                 , slCount      = 0
                 , slLimit      = 0
                 , slPage       = 0
                 , slContent    = map buildElem contents
                 }
        in simpleListing sl route (flip MediaServeR "music") ("Filename", "TODO", "TODO")
    where
  buildElem res = case res of
    MPD.LsDirectory (MPD.Path bs)                       -> let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "directory", T.splitOn "/" p, "", "")
    MPD.LsSong (MPD.Song{MPD.sgFilePath = MPD.Path bs}) -> let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "file", T.splitOn "/" p, "", "")
    MPD.LsPlaylist (MPD.PlaylistName bs)                -> let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "file", T.splitOn "/" p, "", "")

-- | Single Song item Widget.
songSingle :: MPDSec -> [Text] -> MPD.Song -> Widget
songSingle MPDSec{sName = name, sRoute = route} fps MPD.Song
  { MPD.sgFilePath = MPD.Path bs
  , MPD.sgLength = seconds
  } = do
  let path = decodeUtf8 bs
  simpleNav name fps route
  [whamlet|
<div .container-fluid style="padding:0">
  <div .row-fluid>
    <div .span7 .page-element>
      <table>
        <tr>
          <th>Filename
          <td>#{path}
        <tr>
          <th>Length
          <td>#{seconds} seconds
    <div.span5 .page-element>
      <a.btn.btn-primary href="@{MediaServeR ServeAuto name fps}" target="_blank">
        <i.icon-white.icon-play> #
        Auto-open
      <a.btn href="@{MediaServeR ServeForceDownload name fps}">
        <i.icon.icon-download-alt> #
        Download
      <a.btn onclick="window.playlist.to_playlist('#{name}', '#{toPath fps}'); return false">
        Add to playlist
|]
