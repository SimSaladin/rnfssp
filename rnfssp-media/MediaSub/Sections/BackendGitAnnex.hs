{-# LANGUAGE RankNTypes, FlexibleInstances, TypeSynonymInstances, QuasiQuotes, MultiParamTypeClasses, TypeFamilies, OverloadedStrings #-}
module MediaSub.Sections.BackendGitAnnex 
    --( AnnexSec, mkAnnexSec )
        where

import           Prelude hiding (mapM_)
import qualified Prelude as P
import           Utils
import           Control.Arrow (second)
import qualified Data.ByteString.Char8  as BC
import           Data.Maybe (isNothing)
import           Data.Monoid
import           Data.Ord (comparing)
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List      as CL
import           Data.List              hiding  (insert, delete)
import qualified Data.Text              as T
import           Data.Text.Encoding             (decodeUtf8)
import qualified System.FilePath        as FP
import           System.Process
import           Database.Persist.Sql

import           MediaSub.Import hiding (mapM_)
import           MediaSub.Sections.Types
import           MediaSub.Browser

browsableServerRender fps s = renderDefault (sArea s) fps
browsableFetchElems       = fetchElements
browsableFetchPlain fps s = do
    (_fps, GAElem _ fp _) <- fetchFile (sArea s) (FP.joinPath fps)
    return $ sPath s FP.</> fp
browsableFetchPlainR  = fetchFiles
melemToContent (GAElem isdir _fp _mdesc) = (if' isdir "directory" "file", [])
searchableSearchT q s = return . flip ListMany (ListFlat (500, 1) Nothing) $ searchFor s q

    updateMedia sec = differenceSortedE snd
        (lift (dbSource sec $$ CL.consume) >>= CL.sourceList . sort)
        (sourceGitFiles $ sPath sec)
        $$ handler [] [] -- elements to delete, new elements
     where
      -- handler :: [FilePath] -> [(FPS, Html)] -> Sink UpdateTarget (HandlerT master IO) [(FPS, Html)]
      handler todel ra = await >>= maybe (lift (delete' todel) >> return ra) handleElement
        where
            handleElement (Left fp) = handler (todel ++ [fp]) ra
            handleElement (Right e) = do
                new_elem <- lift $ runDB $ case e of
                  (False, path) -> do
                      parent <- findParent path
                      -- FIXME this check shouldn't be necessary!
                      exists <- getBy $ UniqueFNode (sArea sec) path
                      case exists of
                          Nothing -> void $ insert $ FNode (sArea sec) (entityKey <$> parent) path Nothing
                          Just _  -> return ()
                      pathToRecent path

                  (True, path) -> do
                      parent <- findParent path
                      exists <- getBy $ UniqueDNode (sArea sec) path
                      case exists of
                          Nothing -> void $ insert $ DNode (sArea sec) (entityKey <$> parent) path
                          Just _  -> return ()
                      pathToRecent path

                -- reached a directory => delete queue (does this work always?)
                lift $ delete' todel
                handler [] (new_elem : ra)

      findParent     path = getBy $ UniqueDNode (sArea sec) (FP.takeDirectory path)

      delete'          [] = return ()
      delete'       paths = runDB $ P.mapM_ delAll
          =<< selectList [DNodeArea ==. sArea sec, DNodePath <-. paths] [Desc DNodePath]

      delAll (Entity k _) = do deleteWhere [FNodeArea ==. sArea sec, FNodeParent ==. Just k]
                               deleteWhere [DNodeArea ==. sArea sec, DNodeParent ==. Just k]
                               delete k

-- * Query

fetchElements :: FPS -> AnnexSec -> ListViewConf -> MediaView MediaSub AnnexSec
fetchElements fps s (ListFlat mpg _) = case fps of
    [] -> do
        (source, n) <- fetchRoot
        return $ ListMany source (ListFlat mpg $ Just n)
    _  -> runDB (getBy $ UniqueDNode (sArea s) path) >>= \md -> case md of
            Just dir -> do
                (source, n) <- fetchDirectory dir
                return $ ListMany source (ListFlat mpg $ Just n)
            Nothing  -> liftM (ListSingle . snd) $ fetchFile (sArea s) path
  where
    path            = FP.joinPath fps
    fetchRoot       = pagingQuery (sArea s) mpg Nothing
    fetchDirectory  = pagingQuery (sArea s) mpg . Just . entityKey

fetchFile :: SectionId -> FilePath -> HandlerT MediaSub IO (FPS, MElem MediaSub AnnexSec)
fetchFile area = liftM (((,) <$> FP.splitDirectories . fNodePath <*> toGAElem) . entityVal)
    . runDB . getBy404 . UniqueFNode area
    where toGAElem = GAElem <$> const False
                            <*> fNodePath
                            <*> fNodeDetails

pagingQuery :: SectionId -> Paging -> Maybe (Key DNode)
           -> HandlerT MediaSub IO (Source (HandlerT MediaSub IO) (FPS, MElem MediaSub AnnexSec), Int)
pagingQuery secid (limit, offset) mp = liftM ((,) getSource) countQuery
  where
    getSource = runDBSource . mapOutput toElem $ myquery
    myquery   = rawQuery qstring $ toPersistValue secid : maybe [] (\x -> [toPersistValue x]) mp
                                   ++ [ toPersistValue limit, toPersistValue $ limit * offset ]
    qstring   = T.unlines
        [ "SELECT isfile, path, details FROM (SELECT FALSE as isfile, area, path, parent, NULL    as details FROM d_node"
        ,                              "UNION SELECT TRUE  as isfile, area, path, parent, details as details FROM f_node)"
        , "_ WHERE area = ? AND parent "  <> if' (isNothing mp) "IS NULL" "= ?"
        , "ORDER BY isfile, path LIMIT ? OFFSET ?"
        ]
    countQuery = runDB $ liftM2 (+) ( count [FNodeArea ==. secid, FNodeParent ==. mp] )
                                    ( count [DNodeArea ==. secid, DNodeParent ==. mp] )

fetchFiles :: FPS -> AnnexSec -> MediaSource MediaSub FilePath
fetchFiles fps s = runDBSource . mapOutput toFilePath $ rawQuery qstring
    [ toPersistValue secid, toPersistValue secid, toPersistValue $ FP.joinPath fps ]
        where
            secid = sArea s
            qstring =
               "WITH RECURSIVE tr_nodes(id, parent, path) AS ( "
                   <> "WITH nodes AS ( SELECT d.id, d.parent, d.path FROM d_node d WHERE area = ? "
                             <> "UNION SELECT NULL, f.parent, f.path FROM f_node f WHERE area = ? "
                   <> ") SELECT * FROM nodes WHERE path = ? "
                   <> "UNION ALL SELECT n.* FROM tr_nodes nr, nodes n WHERE n.parent = nr.id "
                 <> ") SELECT path FROM tr_nodes WHERE id IS NULL ORDER BY path"

searchFor :: AnnexSec -> Text -> Source (HandlerT MediaSub IO) (FPS, MElem MediaSub AnnexSec)
searchFor sec qtext = runDBSource . mapOutput toElem $ rawQuery (query "") -- TODO limits
        [ toPersistValue (sArea sec)
        , toPersistValue $ ".*" <> qtext <> ".*"
        ] where
      query limits = T.unlines
            [ "SELECT isfile, path, details FROM (SELECT FALSE as isfile, path, area, NULL    as details FROM d_node"
            ,                              "UNION SELECT TRUE  as isfile, path, area, details as details FROM f_node)"
            , "_ WHERE area = ? AND path ~* ? ORDER BY isfile, path" <> limits ]

-- * Update

type FWrap = (Bool, FilePath) -- ^ (Is dir?, relative path)

type UpdateTarget = Either FilePath FWrap -- ^ Right delete_this, Left add_this

pathToRecent :: Monad m => FilePath -> m (FPS, Html)
pathToRecent path = return
    ( FP.splitDirectories path
    , toHtml . f $ FP.splitDirectories path
    ) where f [] = "(empty? this shouldn't be possible)" ++ path
            f xs = last xs
