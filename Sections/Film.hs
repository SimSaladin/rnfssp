------------------------------------------------------------------------------
-- File:          FilmSection.hs
-- Creation Date: Dec 23 2012 [23:15:20]
-- Last Modified: Jul 06 2013 [01:05:44]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

module Sections.Film (FilmSec, mkFilmSec) where

import           Sections
import           Import
import           Utils
import           Data.List            (last)
import qualified Data.Conduit         as C
import qualified Data.Conduit.List    as CL
import qualified Data.Text            as T
import           JSBrowser
import qualified System.FilePath.Find as F
import           System.FilePath.Find ( (/=?) )
import qualified Data.Set             as S
import           System.Posix.Files   (FileStatus, fileSize, modificationTime,
                                      isDirectory, isSymbolicLink,
                                      readSymbolicLink, getFileStatus)
import           System.FilePath ((</>), normalise, splitDirectories)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           System.Process (readProcessWithExitCode)
import           Database.Persist.Sql (rawSql)

import Sections.Types

data FilmSec = FilmSec { sName  :: Text
                       , sPath  :: FilePath
                       , sRoute :: [Text] -> Route App
                       }

-- TODO: create sections from MediaConf
mkFilmSec :: Section -> MediaConf -> FilmSec
mkFilmSec section mc = FilmSec section (mcPath mc) (MediaContentR section)

instance MSection FilmSec where
  sFind        = findFiles
  sWContent    = getContent
  sFilePath    = getFile
  sUpdateIndex = updateListing
  sWSearch     = searchDB

getPages :: Handler (Int, Int)
getPages = do
    limit <- liftM (maybe 50 (read . T.unpack)) $ lookupGetParam "limit_to"
    page  <- liftM (maybe 0 (read . T.unpack)) $ lookupGetParam "page"
    return (limit, page)

-- | GET params:
--  * limit_to  - how many results per page
--  * page      - the page to show
getContent :: FilmSec -> [Text] -> Widget
getContent fsec@FilmSec{sName = sect, sRoute = route} fps = do
    (limit, page) <- liftHandlerT getPages
    let opts = [Desc FilenodeIsdir, Asc FilenodePath, LimitTo limit, OffsetBy $ limit * page]

    -- Left children, Right node
    res <- liftHandlerT . runDB $ case fps of
        []   -> let filters = [FilenodeArea ==. sect, FilenodeParent ==. Nothing]
                    in liftM Left $ do
                        a <- selectList filters opts
                        b <- count filters
                        return (a, b)
        fps' -> do
            -- the node
            node@(Entity key val) <- getBy404 $ UniqueFilenode sect (T.intercalate "/" fps') -- FIXME; '/' as path separator
            if filenodeIsdir val
                then let filters = [FilenodeParent ==. Just key]
                    in liftM Left $ do
                        a <- selectList filters opts
                        b <- count filters
                        return (a, b)
                else return $ Right node
    case res of
        Left (children, siblingCount) ->
            let sl = simpleListingSettings
                    { slSect    = sect
                    , slCurrent = fps
                    , slCount   = siblingCount
                    , slPage    = page
                    , slLimit   = limit
                    , slContent = map (buildElem . entityVal) children
                    }
                in simpleListing sl route (flip MediaServeR sect) ("Filename", "File size", "Modified")
        Right node -> animeSingle fsec fps node

buildElem :: Filenode -> (Text, Text, [Text], Text, Text)
buildElem val = ( last $ T.splitOn "/" $ filenodePath val
                , if' (filenodeIsdir val) "directory" "file"
                , T.splitOn "/" $ filenodePath val
                , filenodeSize val
                , T.pack $ printfTime "%d.%m -%y" $ filenodeModTime val
                )

-- | Single file.
animeSingle :: FilmSec -> [Text] -> Entity Filenode -> Widget
animeSingle FilmSec{sRoute = route, sName = name} fps (Entity _ val) = do
  simpleNav name fps route
  let href = (MediaServeR ServeAuto name fps, [("bar", "aoeu")])
  [whamlet|
<div .ym-gbox .details>
  <div .text-center>
    <div .btn-toolbar>
      <a.btn .btn-primary href=@?{href} target="_blank">
        <i.icon-white.icon-play> #
        Auto-open
      <a.btn href="@{MediaServeR ServeForceDownload name fps}">
        <i.icon.icon-download-alt> #
        Download
      <a.btn onclick="window.playlist.to_playlist('#{name}', ['#{toPath fps}']); return false">
        Add to playlist
  <table .side-headers>
    <tr>
      <th>Filename
      <td>
        <code>#{filenodePath val}
    <tr>
      <th>Size
      <td>#{filenodeSize val}
    <tr>
      <th>Modified
      <td>#{T.pack $ printfTime "%H:%M %d.%m.%Y" $ filenodeModTime val}
$maybe s <- filenodeDetails val
  <div .page-element>
    <h4>Mediainfo
    <pre>#{s}
|]

searchDB :: FilmSec -> Text -> Widget
searchDB s q = do
    (limit, page) <- liftHandlerT getPages
    nodes <- liftHandlerT $ runDB $ rawSql query
        [ toPersistValue $ sName s
        , toPersistValue $ ".*" `mappend` q `mappend` ".*"
        , toPersistValue limit
        , toPersistValue page
        ]
    let sl = simpleListingSettings
            { slSect    = sName s
            , slCurrent = ["Results for " `mappend` q]
            , slCount   = -1
            , slPage    = page
            , slLimit   = limit
            , slContent = map (buildElem . entityVal) nodes
            }
    simpleListing sl (sRoute s) (flip MediaServeR $ sName s) ("Filename", "File size", "Modified")
  where query = T.pack $ unlines 
          [ "SELECT ?? FROM \"filenode\" "
          , "WHERE \"area\" = ? AND \"path\" ~* ? "
          , "ORDER BY \"path\" "
          , "LIMIT ? OFFSET ? "
          ]

-- -- | Recursively find all child files (and only files) of a node.
-- FIXME use a custom query instead?
findFiles :: FilmSec -> [Text] -> Handler [Text]
findFiles FilmSec{sName = sect} paths = runDB $ do
    entities <- selectList [ FilenodeArea ==. sect, FilenodePath <-. paths ] []
    liftM concat $ mapM f entities
  where
    findChildren parentId = do
      children <- selectList [FilenodeParent ==. Just parentId] [Asc FilenodePath]
      liftM ((++) (map (filenodePath . entityVal) $ filter (not . filenodeIsdir . entityVal) children) . concat) $ mapM (findChildren . entityKey) $ filter (filenodeIsdir . entityVal) children

    f (Entity k v) = if filenodeIsdir v
        then findChildren k
        else return [filenodePath v]

getFile :: FilmSec -> Text -> Handler FilePath
getFile FilmSec{sName = sect, sPath = root} file = do
  Entity _ v <- runDB $ getBy404 $ UniqueFilenode sect file
  if filenodeIsdir v
    then invalidArgs ["Downloading directories is not supported."]
    else return $ root </> (T.unpack $ filenodePath v)

-- | Update files to DB from FS.
-- recursively find files and directories in `dir` along with properties
-- TODO: find and update modified (newer than db) entities?
updateListing :: FilmSec -> Handler [RecentlyAdded]
updateListing FilmSec{sPath = dir', sName = section} = do

  now <- timeNow

  -- Get the dir. Follow symbolic link so everything else works.
  dir <- liftIO $ getFileStatus dir' >>= \x -> if isSymbolicLink x
    then readSymbolicLink dir'
    else return dir'

  -- Evil filtering
  let filterp = (F.filePath /=? dir)
      makeRel = dropWhile (== '/') . drop (length dir)
      f (list, set) fi = if F.evalClause filterp fi
          then let tpath = T.pack $ makeRel $ F.infoPath fi
                in (list ++ [(tpath, F.infoStatus fi)], S.insert tpath set)
          else (list, set)

  (filesInFS, filesInFS') <- liftIO $ F.fold F.always f ([], S.empty) dir

  let fileNotInFS = flip S.notMember filesInFS' . filenodePath . entityVal

      insertNode (path,stat) = do
          parent <- getBy $ UniqueFilenode section (takeDirectory' path)
          val <- liftIO $ toFilenode
              (dir </> T.unpack path)
              section
              (entityKey <$> parent)
              (T.pack $ normalise $ T.unpack path)
              stat
          _key <- insert val
          return $ RecentlyAdded now section (map T.pack $ splitDirectories $ T.unpack $ filenodePath val) (toHtml $ filenodePath val)

  -- delete files not found in the filesystem
  -- XXX: disable/hide only?
  _deleted <- runDB $ C.runResourceT $ selectSource [FilenodeArea ==. section] [Desc FilenodePath]
      C.$= CL.filter fileNotInFS
      C.$= CL.map entityKey C.$= CL.mapM delete
      C.$$ CL.consume -- do something with results?

  -- find completely new entities and insert them and their info
  added <- runDB $ C.runResourceT $ CL.sourceList filesInFS
      C.$= CL.mapMaybeM (\x -> liftM (`ifNothing` x) $ getBy $ UniqueFilenode section $ fst x)
      C.$= CL.mapM insertNode
      C.$$ CL.consume

  -- Try to add parents to nodes. XXX: This is needed becouse..?
  fixed <- runDB $ C.runResourceT $ selectSource [FilenodeArea ==. section, FilenodeParent ==. Nothing] []
      C.$= CL.mapM fixParent
      C.$$ CL.consume

  liftIO . print $ "==> # of files fixed: " ++ show (length fixed)
  return added

    where
  fixParent (Entity key val) = do
      parent <- getBy $ UniqueFilenode section (takeDirectory' $ filenodePath val)
      update key [FilenodeParent =. fmap entityKey parent]

ifNothing :: Maybe a -> b -> Maybe b
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
toFilenode _real section parent path stat = do
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
