------------------------------------------------------------------------------
-- File:          MPDSection.hs
-- Creation Date: Dec 24 2012 [00:26:24]
-- Last Modified: Oct 08 2013 [15:05:05]
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
    data MElem App MPDSec = MESong M.Song
                          | MEPath M.Path

    browsableBanner          s   = [whamlet|<i .icon-white .icon-music>#{sName s}|]
    browsableFetchElems  fps s _ = toListing s fps (musicFetch s fps)       -- TODO apply paging
    browsableFetchPlain  fps s   = return . FP.joinPath $ sRoot s : fps     -- TODO check for the file?
    browsableFetchPlainR fps _   = CL.sourceList =<< lift (M.songPaths fps) -- TODO use source?
    browsableServerRender        = musicRender

    browsableJSRender = undefined -- TODO

instance MediaSearchable App MPDSec where
    data MSearch MPDSec = MSearch -- TODO

    searchableSearchT q sec = liftM (flip ListMany (ListFlat (500, 1) Nothing) . build)
        $ mpdSearch sec q
            where
        build = CL.sourceList . map ((,) <$> songFPS <*> MESong)

    searchableJSRender = undefined
    searchableForm = undefined
    searchableSearch = undefined

instance MediaUpdate App MPDSec where
    updateMedia _  = return [] -- TODO: mpd update?

musicFetch :: MPDSec -> FPS -> Source Handler (FPS, MElem App MPDSec)
musicFetch _ fps = lift (liftHandlerT (M.pathContents fps))
    >>= mapOutputMaybe go . CL.sourceList 
        where
            --go :: M.LsResult -> ConduitM () (MElem App MPDSec) Handler ()
            go (M.LsSong      song) = Just (fps ++ [songFilename song], MESong song)
            go (M.LsDirectory path) = Just (fps ++ [pathName     path], MEPath path)
            go                   _  = Nothing

toListing :: MPDSec -> FPS -> Source Handler (FPS, MElem App MPDSec) -> MediaView App MPDSec
toListing _sec _fps source = do
    contents <- source $$ CL.consume -- TODO direct peek at the conduit (instead of intermediate list)?
    return $ case contents of
        [ (_, song@(MESong _)) ] -> ListSingle song
        _                        -> ListMany   source (ListFlat (500, 1) Nothing)

songFilename :: M.Song -> FilePath
songFilename = pathName . M.sgFilePath
songFPS :: M.Song -> [FilePath]
songFPS = pathFilePath . M.sgFilePath
pathName :: M.Path -> FilePath
pathName = last . pathFilePath
pathFilePath :: M.Path -> [FilePath]
pathFilePath (M.Path p) = FP.splitPath . T.unpack $ decodeUtf8 p

-- TODO use section-specific settings!
mpdSearch :: MPDSec -> Text -> Handler [M.Song]
mpdSearch _s q = let dosearch val = M.search $ val M.=? (M.Value $ encodeUtf8 q)
    in liftM (take 100 . foldl union []) . M.execMpd $ mapM dosearch [M.Artist, M.Album, M.Title]

musicRender :: FPS -> MPDSec -> ListContent App MPDSec -> Widget
musicRender fps s content = renderDefault (sName s) fps content MediaContentR MediaServeR

instance MediaRenderDefault App MPDSec where
    melemToContent (MESong s) = ("song", [("length", T.pack . show $ M.sgLength s)])
    melemToContent (MEPath _) = ("directory", [])
