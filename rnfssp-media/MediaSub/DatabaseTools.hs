{-# LANGUAGE NoImplicitPrelude #-}
-- | Some tools to use database with media
module MediaSub.DatabaseTools where

import           Control.Arrow (second)
import qualified Data.ByteString.Char8  as BC
import           Data.Ord (comparing)
import           Data.Conduit
import qualified Data.Conduit.List      as CL
import           Data.List              hiding  (insert, delete)
import qualified Data.Text              as T
import           Data.Text.Encoding             (decodeUtf8)
import qualified System.FilePath        as FP
import           System.Process
import           Database.Persist.Sql

import MediaSub.Import

dbSource :: SectionId -> Source (HandlerT master IO) FilePath
dbSource sid = lift action >>= mapM_ (yield . unSingle)
        where
    action = runDB $ rawSql query [toPersistValue sid]
    query  =  "SELECT path FROM (SELECT path, area FROM f_node"
              <>         " UNION SELECT path, area FROM d_node"
              <>         ") _ WHERE area = ? ORDER BY path"

-- | three fields: 'isfile', 'path', 'maybe desc'
toElem :: [PersistValue] -> (FPS, MElem MediaSub AnnexSec)

toElem [PersistBool isfile, PersistText path, PersistNull]      =
        ( FP.splitDirectories $ unpack path
        , GAElem (not isfile) (unpack path) Nothing)

toElem [PersistBool isfile, PersistText path, PersistText desc] =
        ( FP.splitDirectories $ unpack path
        , GAElem (not isfile) (unpack path) (Just desc))
toElem _ = error "Sections.BackendGitAnnex.toElem: no parse"

toFilePath :: [PersistValue] -> FilePath
toFilePath [PersistText path] = T.unpack path
toFilePath _ = error "toFilePath: no parse"
