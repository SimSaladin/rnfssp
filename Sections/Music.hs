------------------------------------------------------------------------------
-- File:          MPDSection.hs
-- Creation Date: Dec 24 2012 [00:26:24]
-- Last Modified: Apr 19 2013 [11:05:46]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections.Music (MPDSec, mkMPDSec) where

import           Sections
import           Import
import           Utils
import           JSBrowser
import qualified Mpd as M
import           Data.List (last, union)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           System.FilePath ((</>))

import Sections.Types

data MPDSec = MPDSec { sName  :: Text
                     , sRoot  :: FilePath
                     , sRoute :: [Text] -> Route App
                     }

mkMPDSec :: Section -> MediaConf -> MPDSec
mkMPDSec section mc = MPDSec section (mcPath mc) (MediaContentR section)

instance MSection MPDSec where
  sFind _         = M.songPaths
  sWContent       = musicContent
  sWSearch        = searchMPD
  sFilePath MPDSec{sRoot = root} = return . (</>) root . T.unpack
  sUpdateIndex _  = return () -- TODO: mpd update?

musicContent :: MPDSec -> [Text] -> Widget
musicContent sec@MPDSec{sRoute = route} fps = do
    contents <- lift $ M.pathContents fps
    case contents of
        [M.LsSong song] -> songSingle sec fps song
        [M.LsPlaylist (M.PlaylistName bs)] -> undefined
        _ -> let sl = SimpleListingSettings
                     { slSect       = sName sec
                     , slCurrent    = fps
                     , slCount      = 0
                     , slLimit      = 0
                     , slPage       = 0
                     , slContent    = map buildElem contents
                     }
            in simpleListing sl route (flip MediaServeR $ sName sec) ("Filename", "TODO", "TODO")
  where
    buildElem res = case res of
      M.LsDirectory (M.Path bs)                   -> let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "directory", T.splitOn "/" p, "", "")
      M.LsSong (M.Song{M.sgFilePath = M.Path bs}) -> let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "file", T.splitOn "/" p, "", "")
      M.LsPlaylist (M.PlaylistName bs)            -> let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "file", T.splitOn "/" p, "", "")

searchMPD :: MPDSec -> Text -> Widget
searchMPD s q = do
    let dosearch val = M.search $ val M.=? (M.Value $ encodeUtf8 q)
    contents <- liftM (take 100 . foldl union []) . lift . M.execMpd $ mapM dosearch [M.Artist, M.Album, M.Title]
    let sl = SimpleListingSettings
             { slSect       = sName s
             , slCurrent    = ["Results for " `mappend` q]
             , slCount      = 0
             , slLimit      = 0
             , slPage       = 0
             , slContent    = map buildElem' contents
             }
        in simpleListing sl (sRoute s) (flip MediaServeR $ sName s) ("Filename", "TODO", "TODO")
  where
    buildElem' M.Song{M.sgFilePath = M.Path bs} = let
        p = decodeUtf8 bs
        in (last $ T.splitOn "/" p, "file", T.splitOn "/" p, "", "")

-- | Single Song item Widget.
songSingle :: MPDSec -> [Text] -> M.Song -> Widget
songSingle MPDSec{sName = name, sRoute = route} fps M.Song
  { M.sgFilePath = M.Path bs
  , M.sgLength = seconds
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
