{-# LANGUAGE FlexibleInstances #-}
module Sections.BackendGitAnnex 
    --( AnnexSec, mkAnnexSec )
        where

import           Import
import           Utils
import           Sections.Types
import           JSBrowser
import           Control.Arrow (second)
import qualified Data.ByteString.Char8  as BC
import           Data.Maybe (isNothing)
import           Data.Ord (comparing)
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.Internal          (Pipe(..), ConduitM(..))
import qualified Data.Conduit.Internal  as CI
import qualified Data.Conduit.List      as CL
import           Data.List              hiding  (insert, delete)
import qualified Data.Text              as T
import           Data.Text.Encoding             (decodeUtf8)
import qualified System.FilePath        as FP
import           System.Process
import           Database.Persist.Sql

data AnnexSec = AnnexSec
    { sArea  :: Text
    , sPath  :: FilePath
    , sRoute :: FPS -> Route App
    , sScreenname :: Text
    }

mkAnnexSec :: SectionId -> MediaConf -> AnnexSec
mkAnnexSec section mc = AnnexSec section
            (mcPath mc)
            (MediaContentR section)
            section

instance ToJSON (MElem App AnnexSec) where
    toJSON = error "toJSON not implemented" -- TODO implement/derive?

instance MediaBrowsable App AnnexSec where
    data MElem App AnnexSec = GAElem Bool FilePath (Maybe Text) -- ^ Directory? ...
    browsableBanner         s = [whamlet|
<i .icon-white .icon-link>&nbsp;
&nbsp;#{sScreenname s}
|]
    browsableServerRender     = renderElements
    browsableFetchElems       = fetchElements
    browsableFetchPlain fps s = do
        (_fps, GAElem _ fp _) <- fetchFile (sArea s) (FP.joinPath fps)
        return $ sPath s FP.</> fp
    browsableFetchPlainR  = fetchFiles
    browsableJSRender     = error "js render not implemented"  -- TODO Haaa?

-- * Query

fetchElements :: FPS -> AnnexSec -> ListViewConf -> MediaView App AnnexSec
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

fetchFile :: SectionId -> FilePath -> Handler (FPS, MElem App AnnexSec)
fetchFile area = liftM (((,) <$> FP.splitDirectories . fNodePath <*> toGAElem) . entityVal)
    . runDB . getBy404 . UniqueFNode area
    where toGAElem = GAElem <$> const False
                            <*> fNodePath
                            <*> fNodeDetails

pagingQuery :: SectionId -> Paging -> Maybe (Key DNode)
           -> Handler (Source Handler (FPS, MElem App AnnexSec), Int)
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

fetchFiles :: FPS -> AnnexSec -> MediaSource App FilePath
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


-- * Render

instance MediaRenderDefault App AnnexSec where
    melemToContent (GAElem isdir _fp _mdesc) = (if' isdir "directory" "file", [])

renderElements :: FPS -> AnnexSec -> ListContent App AnnexSec -> Widget
renderElements fps s content = renderDefault (sArea s) fps content MediaContentR MediaServeR

-- * Search

instance MediaSearchable App AnnexSec where
    data MSearch AnnexSec = MSearch
    searchableSearchT q s = return . flip ListMany (ListFlat (500, 1) Nothing) $ searchFor s q

searchFor :: AnnexSec -> Text -> Source Handler (FPS, MElem App AnnexSec)
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

instance MediaUpdate App AnnexSec where
  updateMedia sec = differenceSortedE snd
        (lift (dbSource sec $$ CL.consume) >>= CL.sourceList . sort)
        (sourceGitFiles $ sPath sec)
        $$ handler [] [] -- elements to delete, new elements
    where
      handler :: [FilePath] -> [(FPS, Html)] -> Sink UpdateTarget Handler [(FPS, Html)]
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
      delete'       paths = runDB $ mapM_ delAll
          =<< selectList [DNodeArea ==. sArea sec, DNodePath <-. paths] [Desc DNodePath]

      delAll (Entity k _) = do deleteWhere [FNodeArea ==. sArea sec, FNodeParent ==. Just k]
                               deleteWhere [DNodeArea ==. sArea sec, DNodeParent ==. Just k]
                               delete k

pathToRecent :: Monad m => FilePath -> m (FPS, Html)
pathToRecent path = return
    ( FP.splitDirectories path
    , toHtml . f $ FP.splitDirectories path
    ) where f [] = "(empty? this shouldn't be possible)" ++ path
            f xs = last xs

-- | Takes the difference between two sorted sources. Output values are wrapped
-- in Either: unique values from first source are wrapped in Left and vice
-- versa.
differenceSortedE :: (Ord a, Monad m)
           => (b -> a)
           -> Source m a -- ^ Left values
           -> Source m b -- ^ Right values
           -> Source m (Either a b)
differenceSortedE convert (ConduitM db0) (ConduitM git0) = ConduitM $ go db0 git0
  where
    go (Done ())  right          = CI.mapOutput Right right
    go left       (Done ())      = CI.mapOutput Left left
    go (PipeM mx) (PipeM my)     = PipeM (liftM2 go mx my)
    go (PipeM mx) y@HaveOutput{} = PipeM (liftM (`go` y) mx)
    go x@HaveOutput{} (PipeM my) = PipeM (liftM (go x)   my)
    go xs@(HaveOutput srcx closex x) ys@(HaveOutput srcy closey y) =
        case compare x (convert y) of
            EQ -> go srcx srcy                             -- Non-unique entry, discard
            GT -> HaveOutput (go xs srcy) closey (Right y) -- Left (db) is ahead, 
            LT -> HaveOutput (go srcx ys) closex (Left x)  -- left (db) is behind: 
    go (NeedInput _ c) right    = go (c ()) right
    go left (NeedInput _ c)     = go left (c ())
    go (Leftover left ()) right = go left right
    go left (Leftover right ()) = go left right

-- ** Database

dbSource :: AnnexSec -> Source Handler FilePath
dbSource sec = lift action >>= mapM_ (yield . unSingle)
    where
        action = runDB $ rawSql query [toPersistValue (sArea sec)]
        query  =  "SELECT path FROM (SELECT path, area FROM f_node"
                  <>         " UNION SELECT path, area FROM d_node) _ WHERE area = ? ORDER BY path"

-- | three fields: 'isfile', 'path', 'maybe desc'
toElem :: [PersistValue] -> (FPS, MElem App AnnexSec)
toElem [PersistBool isfile, PersistText path, PersistNull]      = (FP.splitDirectories $ T.unpack path, GAElem (not isfile) (T.unpack path) Nothing)
toElem [PersistBool isfile, PersistText path, PersistText desc] = (FP.splitDirectories $ T.unpack path, GAElem (not isfile) (T.unpack path) (Just desc))
toElem _ = error "Sections.BackendGitAnnex.toElem: no parse"

toFilePath :: [PersistValue] -> FilePath
toFilePath [PersistText path] = T.unpack path
toFilePath _ = error "toFilePath: no parse"

-- ** git(-annex)

sourceGitFiles :: MonadIO m
               => FilePath -- ^ Path to repository
               -> Source m (Bool, FilePath)
sourceGitFiles repo = CL.sourceList . sortBy (comparing snd) =<<
    (sourceStdoutOf repo "sh" ["-c", "git ls-tree $(git rev-parse --abbrev-ref HEAD) -rtz"]
                      $= sepOnNull $= toGitElem $$ CL.consume
                      )

toGitElem :: Monad m => Conduit Text m (Bool, FilePath)
toGitElem = CL.mapMaybe $ f . T.words
    where f (_:t:_:xs) = Just ("t" `T.isPrefixOf` t, T.unpack $ T.unwords xs)
          f         [] = Nothing
          f         xs = error $ T.unpack $ "toGitElem: invalid input: " <> T.unwords xs

sepOnNull :: Monad m => Conduit BC.ByteString m Text
sepOnNull = CL.concatMapAccum f BC.empty where
    f bs buffer = second (map decodeUtf8) $ case BC.split '\0' bs of
            []              -> (buffer,   [])
            (x : [])        -> (BC.empty, [buffer <> x])
            (x : xs@(_:_) ) -> (last xs,  (buffer <> x) : init xs)

sourceStdoutOf :: MonadIO m
               => FilePath -> String -> [String]
               -> Source m BC.ByteString
sourceStdoutOf wdir cmd params = cmd' >>= \(_,h,_,_) -> sourceHandle h
  where cmd' = liftIO $ runInteractiveProcess cmd params (Just wdir) Nothing
