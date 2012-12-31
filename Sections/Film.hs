------------------------------------------------------------------------------
-- File:          FilmSection.hs
-- Creation Date: Dec 23 2012 [23:15:20]
-- Last Modified: Dec 31 2012 [08:32:04]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections.Film (FilmSec(..)) where

import           Sections
import           Import
import           Utils
import           Data.List (last)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import           JSBrowser
import qualified System.FilePath.Find as F
import qualified Data.Set as S
import           System.Posix.Files (FileStatus, fileSize, modificationTime, isDirectory)
import           System.FilePath ((</>), normalise)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           System.Process (readProcessWithExitCode)

videoExtensions :: [FilePath]
videoExtensions = [".mkv", ".avi", ".jpg", ".m4a", ".ogm", ".sfv", ".ts"]

data FilmSec = FilmSec { sName  :: Text
                       , sPath  :: FilePath
                       , sRoute :: [Text] -> Route App
                       }

instance MSection FilmSec where
  ident = sName
  findr = findFiles
  content = getContent
  filepath = getFile
  updateIndex = updateListing

getContent :: FilmSec -> [Text] -> Widget
getContent fsec@FilmSec{sName = sect, sRoute = route} fps = do
  (mnode, mchildren) <- lift . runDB $ case fps of
      []   -> liftM ((,) Nothing . Just) $ selectList
          [FilenodeArea ==. sect, FilenodeParent ==. Nothing]
          [Asc FilenodePath]
      fps' -> do
        Entity key val <- getBy404 $ UniqueFilenode sect (T.intercalate "/" fps') -- FIXME; '/' as path separator
        liftM ((,) $ Just $ Entity key val) $ if filenodeIsdir val
          then liftM Just $ selectList [FilenodeParent ==. Just key] [Asc FilenodePath]
          else return Nothing
  case (mnode, mchildren) of
    (_, Just children) -> simpleListing sect
                                        fps
                                        (map (buildElem . entityVal) children)
                                        route
                                        (flip MediaServeR sect)
                                        ("Filename", "File size", "Modified")
    (Just node, _) -> animeSingle fsec fps node
    _ -> lift notFound
    where
  buildElem val = ( last $ T.splitOn "/" $ filenodePath val
                  , if' (filenodeIsdir val) "directory" "file"
                  , T.splitOn "/" $ filenodePath val
                  , filenodeSize val
                  , T.pack $ printfTime "%d.%m -%y" $ filenodeModTime val
                  )

-- | Single file.
animeSingle :: FilmSec -> [Text] -> Entity Filenode -> Widget
animeSingle FilmSec{sRoute = route, sName = name} fps (Entity _ val) = do
  simpleNav fps route
  [whamlet|
<div .container-fluid style="padding:0">
  <div .row-fluid>
    <div .span7 .page-element>
      <table>
        <tr>
          <th>Filename
          <td><i>#{filenodePath val}
        <tr>
          <th>Size
          <td>#{filenodeSize val}
        <tr>
          <th>Modified
          <td>#{T.pack $ printfTime "%d.%m -%y" $ filenodeModTime val}
    <div.span5 .page-element>
      <a.btn.btn-primary href="@{MediaServeR "auto" name fps}" target="_blank">
        <i.icon-white.icon-play> #
        Auto-open
      <a.btn href="@{MediaServeR "force" name fps}">
        <i.icon.icon-download-alt> #
        Download
      <a.btn onclick="window.playlist.to_playlist('#{name}', '#{toPath fps}'); return false">
        Add to playlist
$maybe s <- filenodeDetails val
  <div .page-element>
    <h4>Mediainfo
    <pre>#{s}
  |]

-- -- | Recursively find all child files (and only files) of a node.
-- FIXME use a custom query instead?
findFiles :: FilmSec -> Text -> Handler [Text]
findFiles FilmSec{sName = sect} path = runDB $ do
    Entity k v <- getBy404 $ UniqueFilenode sect path
    if filenodeIsdir v
      then findChildren k
      else return [filenodePath v]
  where
    findChildren parentId = do
      children <- selectList [FilenodeParent ==. Just parentId] [Asc FilenodePath]
      liftM ((++) (map (filenodePath . entityVal) $ filter (not . filenodeIsdir . entityVal) children) . concat) $ mapM (findChildren . entityKey) $ filter (filenodeIsdir . entityVal) children

getFile :: FilmSec -> Text -> Handler FilePath
getFile FilmSec{sName = sect} path = do
  Entity _ v <- runDB $ getBy404 $ UniqueFilenode sect path
  if filenodeIsdir v
    then invalidArgs ["Downloading directories is not supported."]
    else toFSPath sect $ T.unpack $ filenodePath v

-- | Update files to DB from FS.
-- recursively find files and directories in `dir` along with properties
-- TODO: find and update modified (newer than db) entities?
updateListing :: FilmSec -> Handler ()
updateListing FilmSec{sPath = dir, sName = section} = do
  let filterp = foldl (F.||?) (F.fileType F.==? F.Directory F.&&? F.filePath F./=? dir)
                      $ map (F.extension F.==?) videoExtensions
      makeRel = dropWhile (== '/') . drop (length dir)

  (filesInFS, filesInFS') <- liftIO $ F.fold
      F.always
      (\(list, set) fi -> if F.evalClause filterp fi
          then let tpath = T.pack $ makeRel $ F.infoPath fi
            in (list ++ [(tpath, F.infoStatus fi)], S.insert tpath set)
          else (list, set))
      ([], S.empty)
      dir

  let fileNotInFS = flip S.notMember filesInFS' . filenodePath . entityVal

  liftIO . print $ "==> # of elems in filesInFS: " ++ show (length filesInFS) -- DEBUG

  -- delete files not found in the filesystem
  -- XXX: disable/hide only?
  deleted <- runDB $ C.runResourceT $ selectSource [FilenodeArea ==. section] [Desc FilenodePath]
      C.$= CL.filter fileNotInFS
      C.$= CL.map entityKey C.$= CL.mapM delete
      C.$$ CL.consume -- do something with results?

  liftIO . print $ "==> Deleted entries: " ++ show (length deleted)

  -- find completely new entities and insert them and their info
  added <- runDB $ C.runResourceT $ CL.sourceList filesInFS
    C.$= CL.mapMaybeM (\x -> liftM (`ifNothing` x) $ getBy $ UniqueFilenode section $ fst x)
    C.$= CL.mapM insertNode
    C.$$ CL.consume

  liftIO . print $ "==> # of files added: " ++ show (length added)

  -- Try to add parents to nodes. XXX: This is needed becouse..?
  fixed <- runDB $ C.runResourceT $ selectSource [FilenodeArea ==. section, FilenodeParent ==. Nothing] []
      C.$= CL.mapM fixParent
      C.$$ CL.consume

  liftIO . print $ "==> # of files fixed: " ++ show (length fixed)
  return ()
    where
  insertNode (path,stat) = do
      parent <- getBy $ UniqueFilenode section (takeDirectory' path)
      insert =<< liftIO (toFilenode (dir </> T.unpack path)
                                    section
                                    (entityKey <$> parent)
                                    (T.pack $ normalise $ T.unpack path)
                                    stat)

  fixParent (Entity key val) = do
      parent <- getBy $ UniqueFilenode section (takeDirectory' $ filenodePath val)
      update key [FilenodeParent =. fmap entityKey parent]

  ifNothing mx x = case mx of
      Nothing -> Just x
      Just _  -> Nothing

-- | Generate a filenode from properties.
toFilenode :: FilePath         -- ^ Real path to the file
           -> Text             -- ^ area
           -> Maybe FilenodeId -- ^ parent node
           -> Text             -- ^ path of the node
           -> FileStatus       -- ^ status of the node
           -> IO Filenode
toFilenode real section parent path stat = do
    -- details <- if' isdir (return Nothing) $ Just <$> getDetails real
    let details = Nothing
    return $ Filenode section parent isdir path
                      (prettyFilesize $ fileSize stat)
                      (posixSecondsToUTCTime $ realToFrac $ modificationTime stat)
                      details
    where isdir = isDirectory stat

-- |
getDetails :: FilePath -> IO Text
getDetails fp = do
  (_, out, _) <- readProcessWithExitCode "mediainfo" [fp] ""
  return $ T.pack out

