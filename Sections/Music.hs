------------------------------------------------------------------------------
-- File:          MPDSection.hs
-- Creation Date: Dec 24 2012 [00:26:24]
-- Last Modified: Oct 03 2013 [02:10:56]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections.Music (MPDSec(..), mkMPDSec) where

import           Import
import qualified Network.MPD as M
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           JSBrowser
import qualified Mpd as M
import           Data.List (last, union)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           System.FilePath as FP

import Sections.Types

data MPDSec = MPDSec
    { sName  :: Text
    , sRoot  :: FilePath
    , sRoute :: FPS -> Route App
    }

mkMPDSec :: SectionId -> MediaConf -> MPDSec
mkMPDSec section mc = MPDSec section (mcPath mc) (MediaContentR section)

instance MediaBrowsable App MPDSec where
    data MElem MPDSec = MESong M.Song
                      | MEPath M.Path

    browsableBanner          s   = [whamlet|<i .icon-white .icon-music>#{sName s}|]
    browsableFetchElems  fps s _ = toListing s fps (musicFetch s fps)       -- TODO apply paging
    browsableFetchPlain  fps s   = return . FP.joinPath $ sRoot s : fps     -- TODO check for the file?
    browsableFetchPlainR fps _   = CL.sourceList =<< lift (M.songPaths fps) -- TODO use source?
    browsableServerRender        = musicRender

    browsableJSRender = undefined -- TODO

instance MediaSearchable App MPDSec where
    data MSearch MPDSec = MSearch -- TODO

    searchableSearchT q sec = return $ ListFlat sec []
        $ lift (mpdSearch sec q) >>= CL.sourceList . map MESong

    searchableJSRender = undefined
    searchableForm = undefined
    searchableSearch = undefined

instance MediaUpdate App MPDSec where
    updateMedia _  = return [] -- TODO: mpd update?

musicFetch :: MPDSec -> FPS -> Source Handler (MElem MPDSec)
musicFetch _ fps = lift (liftHandlerT (M.pathContents fps))
    >>= mapOutputMaybe go . CL.sourceList 
        where
            --go :: M.LsResult -> ConduitM () (MElem MPDSec) Handler ()
            go (M.LsSong      song) = Just (MESong song)
            go (M.LsDirectory path) = Just (MEPath path)
            go                   _  = Nothing

toListing :: MPDSec -> FPS -> Source Handler (MElem MPDSec) -> MediaView App MPDSec
toListing sec fps source = do
    contents <- source $$ CL.consume -- TODO direct peek at the conduit (instead of intermediate list)
    return $ case contents of
        [song@(MESong _)] -> ListSingle sec fps song
        _                 -> ListFlat   sec fps source


-- TODO use section!
mpdSearch :: MPDSec -> Text -> Handler [M.Song]
mpdSearch _s q = let dosearch val = M.search $ val M.=? (M.Value $ encodeUtf8 q)
    in liftM (take 100 . foldl union []) . M.execMpd $ mapM dosearch [M.Artist, M.Album, M.Title]

musicRender :: ListContent MPDSec -> Widget
musicRender (ListSingle MPDSec{sName = name, sRoute = route} fps (MESong song)) =
    let seconds   = M.sgLength song
        M.Path bs = M.sgFilePath song
        path      = decodeUtf8 bs
        in do
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
musicRender (ListFlat _s _fps _source) = undefined
--            sl = SimpleListingSettings
--                { slSect       = sName sec
--                , slCurrent    = fps
--                , slCount      = 0
--                , slLimit      = 0
--                , slPage       = 0
--                , slContent    = map buildElem contents
--                }
--            in simpleListing sl route (flip MediaServeR $ sName sec) ("Filename", "TODO", "TODO")
  where
    --buildElem res = case res of
    buildElem (MEPath (M.Path bs))                        = let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "directory", map T.unpack $ T.splitOn "/" p, "", "")
    buildElem (MESong (M.Song{M.sgFilePath = M.Path bs})) = let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "file", map T.unpack $ T.splitOn "/" p, "", "")
--      M.LsPlaylist (M.PlaylistName bs)            -> let p = decodeUtf8 bs in (last $ T.splitOn "/" p, "file", map T.unpack $ T.splitOn "/" p, "", "")
musicRender _ = undefined

-- * HUH?

-- TODO huh?
_searchMPDWidget :: MPDSec -> Text -> Widget
_searchMPDWidget s q = do
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
