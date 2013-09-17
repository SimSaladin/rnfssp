------------------------------------------------------------------------------
-- File:          MPDSection.hs
-- Creation Date: Dec 24 2012 [00:26:24]
-- Last Modified: Sep 17 2013 [05:08:36]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections.Music (MPDSec(..), mkMPDSec) where

import           Import
import qualified Network.MPD as M
import           Utils
import Data.Conduit
import qualified Data.Conduit.List as CL
import           JSBrowser
import qualified Mpd as M
import           Data.List (last, union)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           System.FilePath as FP

import Sections.Types

data MPDSec = MPDSec { sName  :: Text
                     , sRoot  :: FilePath
                     , sRoute :: FPS -> Route App
                     }

mkMPDSec :: SectionId -> MediaConf -> MPDSec
mkMPDSec section mc = MPDSec section (mcPath mc) (MediaContentR section)

instance MediaBrowsable App MPDSec where
    data MElem MPDSec = MESong M.Song
                      | MEPath M.Path

    browsableFetchPlainR fps                    s = CL.sourceList
        =<< (lift $ M.songPaths fps)

    browsableFetchPlain  fps MPDSec{sRoot = root} =
        return $ FP.joinPath $ root : fps

    browsableBanner s = [whamlet|
<i .icon-white .icon-music>#{sName s}
|]

    browsableFetchElems fps s = musicFetch s fps

    browsableRender source fps s = musicRender s fps source

instance MediaSearchable App MPDSec where

    searchableSearchT q msec = CL.sourceList
        =<< (liftM (map MESong) $ lift $ mpdSearch msec q)

instance MediaUpdate App MPDSec where
    updateMedia _  = return [] -- TODO: mpd update?

musicFetch :: MPDSec -> FPS -> Source Handler (MElem MPDSec)
musicFetch _ fps = lift (liftHandlerT (M.pathContents fps))
    >>= mapOutputMaybe go . CL.sourceList 
        where
            --go :: M.LsResult -> ConduitM () (MElem MPDSec) Handler ()
            go (M.LsSong song) = Just (MESong song)
            go              _  = Nothing

musicRender :: MPDSec -> FPS -> Source Handler (MElem MPDSec) -> Widget
musicRender sec@MPDSec{sRoute = route} fps source = do
    contents <- liftHandlerT $ source $$ CL.consume
    case contents of
        [MESong song] -> songSingle sec fps song
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
    --buildElem res = case res of
    buildElem (MEPath (M.Path bs))                        = let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "directory", map T.unpack $ T.splitOn "/" p, "", "")
    buildElem (MESong (M.Song{M.sgFilePath = M.Path bs})) = let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "file", map T.unpack $ T.splitOn "/" p, "", "")
--      M.LsPlaylist (M.PlaylistName bs)            -> let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "file", map T.unpack $ T.splitOn "/" p, "", "")


mpdSearch :: MPDSec -> Text -> Handler [M.Song]
mpdSearch s q = let dosearch val = M.search $ val M.=? (M.Value $ encodeUtf8 q)
    in liftM (take 100 . foldl union []) . M.execMpd $ mapM dosearch [M.Artist, M.Album, M.Title]

searchMPDWidget :: MPDSec -> Text -> Widget
searchMPDWidget s q = do
    contents <- liftHandlerT $ mpdSearch s q
    let sl = SimpleListingSettings
             { slSect       = sName s
             , slCurrent    = [T.unpack $ "Results for " <> q]
             , slCount      = 0
             , slLimit      = 0
             , slPage       = 0
             , slContent    = map buildElem' contents
             }
        in simpleListing sl (sRoute s) (flip MediaServeR $ sName s) ("Filename", "TODO", "TODO")
  where
    buildElem' M.Song{M.sgFilePath = M.Path bs} = let
        p = decodeUtf8 bs
        in (last $ T.splitOn "/" p, "file", map T.unpack $ T.splitOn "/" p, "", "")

-- | Single Song item Widget.
songSingle :: MPDSec -> FPS -> M.Song -> Widget
songSingle MPDSec{sName = name, sRoute = route} fps M.Song
  { M.sgFilePath = M.Path bs
  , M.sgLength = seconds
  } = do
  let path = decodeUtf8 bs
  simpleNav name fps route
  [whamlet|
<section .ym-gbox>
 <div .site-block>
    <h1>#{path}
    <div .text-center>
      <a .btn .btn-primary href="@{MediaServeR ServeAuto name fps}" target="_blank">
        \<i.icon-white.icon-play></i> Auto-open
      <a .btn href="@{MediaServeR ServeForceDownload name fps}">
        \<i .icon .icon-download-alt></i> Download
      <a .btn onclick="window.playlist.to_playlist('#{name}', ['#{FP.joinPath fps}']); return false">
        Add to playlist
 <section>
      <h1>Details
      <table>
        <tr>
          <th>Filename
          <td>#{path}
        <tr>
          <th>Length
          <td>#{seconds} seconds
|]
