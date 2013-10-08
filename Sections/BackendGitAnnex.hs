{-# LANGUAGE FlexibleInstances #-}
module Sections.BackendGitAnnex 
    ( AnnexSec, mkAnnexSec
    ) where

import           Import
import           Utils
import           Sections.Types
import           JSBrowser
import qualified Data.ByteString.Char8  as BC
import           Data.Maybe (isNothing)
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
    }

mkAnnexSec :: SectionId -> MediaConf -> AnnexSec
mkAnnexSec section mc = AnnexSec section (mcPath mc) (MediaContentR section)

instance ToJSON (MElem App AnnexSec) where
    toJSON = error "toJSON not implemented" -- TODO implement/derive?

instance MediaBrowsable App AnnexSec where
    data MElem App AnnexSec = GAElem Bool FilePath (Maybe Text) -- ^ Directory? ...

    browsableBanner         s = [whamlet|<i .icon-white .icon-link>#{sArea s}|]
    browsableFetchElems       = fetchElements
    browsableFetchPlain fps s = do
        (_fps, GAElem _ fp _) <- fetchFile (sArea s) (FP.joinPath fps)
        return $ sPath s FP.</> fp
    browsableFetchPlainR  = error "fetchplainR not implemented" -- TODO Huh?
    browsableServerRender = renderElements
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
    fetchRoot       = fetchQuery (sArea s) mpg Nothing
    fetchDirectory  = fetchQuery (sArea s) mpg . Just . entityKey

fetchFile :: SectionId -> FilePath -> Handler (FPS, MElem App AnnexSec)
fetchFile area = liftM ( ((,) <$> FP.splitPath . fNodePath <*> toGAElem) . entityVal)
    . runDB . getBy404 . UniqueFNode area
    where toGAElem = GAElem <$> const True
                            <*> fNodePath
                            <*> fNodeDetails

fetchQuery :: SectionId -> Paging -> Maybe (Key DNode) -> Handler (Source Handler (FPS, MElem App AnnexSec), Int)
fetchQuery secid paging mp = liftM ( (,) getSource ) . liftM length . runDB $ myquery $$ CL.consume
    
  where
    getSource = runDBSource . mapOutput toElem $ myquery
        
    myquery = rawQuery qstring $ case mp of
        Nothing ->        replicate 2 (toPersistValue secid)
        Just p  -> join $ replicate 2 [toPersistValue secid, toPersistValue p]

    qstring = T.pack $ unlines
        [ "( SELECT FALSE as ts, path as paths, NULL    FROM d_node WHERE area = ? AND parent " <> pval
        , ") UNION"
        , "( SELECT TRUE  as ts, path as paths, details FROM f_node WHERE area = ? AND parent " <> pval
        , ") ORDER BY ts, paths" <> limits ]

    pval   = if' (isNothing mp) "IS NULL" "= ?"

    limits = case paging of
        (limit, offset) -> " LIMIT " <> show limit <> " OFFSET " <> show (limit * offset)

-- * Render

renderElements :: FPS -> AnnexSec -> ListContent App AnnexSec -> Widget
-- renderElements fps s (ListSingle (GAElem True path desc)) = renderSingle  s path desc
renderElements fps s = renderDefault (sArea s) fps
--    $maybe desc <- mdesc
--        <section>
--            <h1>Mediainfo
--            <pre>#{desc}
--    $nothing
--        <i>Details not available.

instance MediaRenderDefault App AnnexSec where
    melemToContent (GAElem isdir _fp _mdesc) = (if' isdir "directory" "file", [])

-- * Search

instance MediaSearchable App AnnexSec where
    data MSearch AnnexSec = MSearch
    searchableSearchT q s = return . flip ListMany (ListFlat (500, 1) Nothing) $ searchFor s q

searchFor :: AnnexSec -> Text -> Source Handler (FPS, MElem App AnnexSec)
searchFor sec qtext = do
    (limit, offset) <- lift pagerSettings
    runDBSource $ mapOutput toElem $ rawQuery query
        $ (join . replicate 2) [ toPersistValue (sArea sec), toPersistValue $ ".*" <> qtext <> ".*" ]
        -- ++ map toPersistValue [limit, limit * offset] -- TODO
  where
      query = T.pack $ unlines
            [ "(SELECT FALSE as ts, path as paths, NULL FROM d_node"
            , " WHERE area = ? AND path ~* ?"
            , ") UNION"
            , "(SELECT TRUE as ts, path as paths, details FROM f_node"
            -- , " WHERE area = ? AND path ~* ?" -- TODO implement
            , ") ORDER BY ts, paths LIMIT ? OFFSET ? " ]


-- * Update

-- | File node wrapper 
data FWrap = FFile FilePath
           | FDir  FilePath
        deriving (Show)

unFWrap :: FWrap -> FilePath
unFWrap (FFile fp) = fp
unFWrap (FDir  fp) = fp

type UpdateTarget = Either FilePath FWrap

instance MediaUpdate App AnnexSec where
  updateMedia sec = differenceSortedE unFWrap
        (dbSource sec) (gitGetFileList $ sPath sec)
        $$ handler [] []
    where
      handler :: [FilePath] -> [(FPS, Html)] -> Sink UpdateTarget Handler [(FPS, Html)]
      handler todel ra = await >>= maybe (lift (delete' todel) >> return ra) handleElement
        where
            handleElement (Left fp) = handler (todel ++ [fp]) ra
            handleElement (Right e) = do
              ra' <- lift $ runDB $ case e of

                  FFile path -> do
                      parent <- findParent path
                      -- FIXME this check shouldn't be necessary!
                      exists <- getBy $ UniqueFNode (sArea sec) path
                      case exists of
                          Nothing -> void $ insert $ FNode (sArea sec) (entityKey <$> parent) path Nothing
                          Just _  -> return ()
                      pathToRecent path

                  FDir path -> do
                      parent <- findParent path
                      exists <- getBy $ UniqueDNode (sArea sec) path
                      case exists of
                          Nothing -> void $ insert $ DNode (sArea sec) (entityKey <$> parent) path
                          Just _  -> return ()
                      pathToRecent path

              lift $ delete' todel -- reached a directory => delete queue (does this work always?)
              handler [] (ra' : ra)

      findParent     path = getBy $ UniqueDNode (sArea sec) (FP.takeDirectory path)
      delAll (Entity k _) = do deleteWhere [FNodeArea ==. sArea sec, FNodeParent ==. Just k]
                               deleteWhere [DNodeArea ==. sArea sec, DNodeParent ==. Just k]
                               delete k
      delete'          [] = return ()
      delete'       paths = runDB $ mapM_ delAll
          =<< selectList [DNodeArea ==. sArea sec, DNodePath <-. paths] [Asc DNodePath]

pathToRecent :: Monad m => FilePath -> m (FPS, Html)
pathToRecent path = return
    ( FP.splitPath path
    , toHtml $ last $ FP.splitPath path
    )

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
        query  = T.pack $ unlines
            [ "SELECT path"
            , "FROM (SELECT path, area FROM f_node"
            , "UNION SELECT path, area FROM d_node) _ WHERE area = ? ORDER BY path"
            ]

toElem :: [PersistValue] -> (FPS, MElem App AnnexSec)
toElem [PersistBool isfile, PersistText path, PersistNull]      = (FP.splitPath $ T.unpack path, GAElem isfile (T.unpack path) Nothing)
toElem [PersistBool isfile, PersistText path, PersistText desc] = (FP.splitPath $ T.unpack path, GAElem isfile (T.unpack path) (Just desc))
toElem _ = error "Sections.BackendGitAnnex.toElem: invalid value."

-- ** git(-annex)

gitGetFileList :: MonadIO m
               => FilePath -- ^ Path to repository
               -> Source m FWrap
gitGetFileList repo = sourceCmdLines repo "sh"
    ["-c", "git ls-tree -r $(git rev-parse --abbrev-ref HEAD) --name-only"]
    $= explodeSortPaths

explodeSortPaths :: MonadIO m => Conduit FilePath m FWrap
explodeSortPaths = explode' [] where
    explode' fs = do
        mpath <- await
        case mpath of
            Nothing   -> mempty
            Just path -> handleParts [] (FP.splitDirectories path) fs
        where
            -- (yielded already) (git) (stack)
            handleParts pos  xs     []  = do
                mapM_ yield $ composeSubPaths pos xs
                explode' $ init (pos ++ xs)

            handleParts pos (x:xs) (y:ys)
                | x == y    = handleParts (pos ++ [x])     xs ys
                | otherwise = handleParts  pos         (x:xs) []
            handleParts _ _ _ = return () -- FIXME: ????? This case shouldn't be necessary

sourceCmdLines :: MonadIO m
               => FilePath    -- ^ Working directory
               -> String      -- ^ Command
               -> [String]    -- ^ Parameters
               -> Source m FilePath
sourceCmdLines path cmd params =
    (cmd' >>= sourceHandle) $= CL.concatMapAccum getOutLines BC.empty
  where cmd' = liftM (\(_,h,_,_) -> h) . liftIO $
                runInteractiveProcess cmd params (Just path) Nothing

getOutLines :: BC.ByteString -- ^ New input
            -> BC.ByteString -- ^ Buffer. Guaranteed to not have newlines
            -> (BC.ByteString, [FilePath])
getOutLines bs buffer = let
    (x:xs) = BC.split '\n' bs
    in (last xs, map (T.unpack . decodeUtf8) $ (buffer <> x) : init xs)

composeSubPaths :: [FilePath] -> [FilePath] -> [FWrap]
composeSubPaths xs ys = let
    paths = tail $ inits ys
    dirs  = init paths
    file  = last paths
    in map (FDir . FP.joinPath . (++) xs) dirs ++ [FFile $ FP.joinPath $ xs ++ file]
