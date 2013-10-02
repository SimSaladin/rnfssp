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

instance ToJSON (MElem AnnexSec) where
    toJSON = undefined -- TODO implement/derive?

-- * Browse

instance MediaBrowsable App AnnexSec where
    data MElem AnnexSec = GAElem Bool FilePath (Maybe Text) -- ^ Directory? ...

    browsableBanner         s = [whamlet|<i .icon-white .icon-link>#{sArea s}|]
    browsableFetchElems       = fetchElements
    browsableFetchPlain fps s = do
        GAElem _ fp _ <- fetchFile (sArea s) (FP.joinPath fps)
        return $ sPath s FP.</> fp
    browsableFetchPlainR  = undefined -- TODO Huh?
    browsableServerRender = renderElements
    browsableJSRender     = undefined -- TODO Haaa?

fetchElements :: FPS -> AnnexSec -> Paging -> MediaView App AnnexSec
fetchElements fps s mpg = case fps of
    [] -> return $ ListBlocks s fps fetchRoot
    _  -> do
        md <- runDB . getBy $ UniqueDNode (sArea s) path
        case md of
            Just dir -> return $ ListFlat s fps $ fetchDirectory dir
            Nothing  -> liftM (ListSingle s fps) $ fetchFile (sArea s) path
  where
    path            = FP.joinPath fps
    fetchRoot       = fetchQuery (sArea s) mpg Nothing
    fetchDirectory  = fetchQuery (sArea s) mpg . Just . entityKey

fetchFile :: SectionId -> FilePath -> Handler (MElem AnnexSec)
fetchFile area = liftM ((GAElem <$> const True <*> fNodePath <*> fNodeDetails) . entityVal)
    . runDB . getBy404 . UniqueFNode area

fetchQuery :: SectionId -> Paging -> Maybe (Key DNode) -> Source Handler (MElem AnnexSec)
fetchQuery secid paging mp = runDBSource . mapOutput toElem . rawQuery qstring $ case mp of
    Nothing ->        replicate 2 (toPersistValue secid)
    Just p  -> join $ replicate 2 [toPersistValue secid, toPersistValue p]
  where
    qstring = T.pack $ unlines
        [ "( SELECT FALSE as ts, path as paths, NULL    FROM d_node WHERE area = ? AND parent " <> pval
        , ") UNION"
        , "( SELECT TRUE  as ts, path as paths, details FROM f_node WHERE area = ? AND parent " <> pval
        , ") ORDER BY ts, paths" <> limits ]

    pval   = if' (isNothing mp) "IS NULL" "= ?"
    limits = case paging of
        Nothing              -> ""
        Just (offset, limit) -> " LIMIT " <> show limit <> " OFFSET " <> show offset

--renderElements' :: AnnexSec -> (Int, Int) -> FPS -> Source Handler (MElem AnnexSec) -> Widget
--renderElements' s pager fps source = do
--    elems <- liftHandlerT $ source $$ CL.consume
--    case elems of
--        (GAElem True path desc : []) -> renderSingle  s path desc
--        xs                           -> renderListing s pager fps xs

renderElements :: ListContent AnnexSec -> Widget
renderElements (ListSingle s _   (GAElem True path desc)) = renderSingle  s path desc
renderElements (ListFlat   s fps                  source) = renderListing s fps source

renderSingle :: AnnexSec -> FilePath -> Maybe Text -> Widget
renderSingle s path mdesc = do
    let section = sArea s
        fps     = FP.splitPath path
        hauto   = (MediaServeR ServeAuto          section fps, [])
        hforce  =  MediaServeR ServeForceDownload section fps
    simpleNav section fps (sRoute s)
    [whamlet|
<section .ym-gbox .details>
    <h1>#{path}
    <div .text-center>
        <div .btn-group>
          <a .btn .btn-primary href="@?{hauto}" target="_blank">
            \<i .icon-white .icon-play></i> Auto-open
          <a .btn href="@{hforce}">
            \<i .icon .icon-download-alt></i> Download
          <a .btn onclick="window.playlist.to_playlist('#{section}', ['#{path}']); return false">
            To playlist
    $maybe desc <- mdesc
        <section>
            <h1>Mediainfo
            <pre>#{desc}
    $nothing
        <i>Details not available.
|]

renderListing :: AnnexSec -> FPS -> MediaSource app (MElem AnnexSec) -> Widget
renderListing s fps xs = undefined
--    let sl = simpleListingSettings
--                { slSect    = sArea s
--                , slCurrent = fps
--                , slCount   = length xs
--                , slPage    = page
--                , slLimit   = limit
--                , slContent = map buildElem xs
--                }
--        buildElem (GAElem isfile path _) =
--            ( T.pack path
--            , if' isfile "file" "directory"
--            , FP.splitPath path
--            , "", "")
--    simpleListing sl (sRoute s)
--                     (flip MediaServeR $ sArea s)
--                     ("Filename", "File size", "Modified")

toElem :: [PersistValue] -> MElem AnnexSec
toElem [PersistBool isfile, PersistText path, PersistNull]      = GAElem isfile (T.unpack path) Nothing
toElem [PersistBool isfile, PersistText path, PersistText desc] = GAElem isfile (T.unpack path) (Just desc)
toElem _ = error "Sections.BackendGitAnnex.toElem: invalid value."

-- | Get pager settings.
pagerSettings :: Handler (Int, Int) -- TODO: move somewhere (browser? sections helpers?)
pagerSettings = do
    limit <- liftM (maybe 50 (read . T.unpack)) $ lookupGetParam "limit_to"
    page  <- liftM (maybe 0  (read . T.unpack)) $ lookupGetParam "page"
    return (limit, page)

-- * Search

instance MediaSearchable App AnnexSec where
    searchableSearchT q s = (return . ListFlat s []) $ searchFor s q

searchFor :: AnnexSec -> Text -> Source Handler (MElem AnnexSec)
searchFor sec qtext = do
    (limit, offset) <- lift pagerSettings
    runDBSource $ mapOutput toElem $ rawQuery query
        $ (join . replicate 2) [ toPersistValue (sArea sec), toPersistValue $ ".*" <> qtext <> ".*" ]
        ++ map toPersistValue [limit, offset]
  where
      query = T.pack $ unlines
            [ "(SELECT FALSE as ts, path as paths, NULL FROM d_node"
            , " WHERE area = ? AND path ~* ?"
            , ") UNION"
            , "(SELECT TRUE as ts, path as paths, details FROM f_node"
            , " WHERE area = ? AND path ~* ?"
            , ") ORDER BY ts, paths LIMIT ? OFFSET ? " ]


-- * Update

type UpdateTarget = Either FilePath FWrap

instance MediaUpdate App AnnexSec where
  updateMedia sec = differenceSortedE unFWrap
        (dbSource sec) (gitGetFileList (sPath sec) "")
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

-- * Database

-- | File node wrapper 
data FWrap = FFile FilePath
           | FDir  FilePath
        deriving (Show)

unFWrap :: FWrap -> FilePath
unFWrap (FFile fp) = fp
unFWrap (FDir  fp) = fp

dbSource :: AnnexSec -> Source Handler FilePath
dbSource sec = mapM_ (yield . unSingle) =<< lift action
    where
        action = runDB . rawSql query
                       . replicate 2 $ toPersistValue (sArea sec)

        query = T.pack $ unlines
            [ "SELECT path FROM f_node WHERE area = ?"
            , "UNION"
            , "SELECT path FROM d_node WHERE area = ?"
            , "ORDER BY path"
            ]

-- * GIT

gitGetFileList :: MonadIO m
               => FilePath -- ^ Path to repository
               -> FilePath
               -> Source m FWrap
gitGetFileList repo fp = gitSource (Just repo) ["annex", "find", fp]
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

gitSource :: MonadIO m
          => Maybe FilePath -- ^ Path to repository
          -> [String] -> Source m FilePath
gitSource repo params =
    (git >>= sourceHandle) $= CL.concatMapAccum getOutLines BC.empty
  where
      git = liftIO $ liftM (\(_,h,_,_) -> h) $
          runInteractiveProcess "git" params repo Nothing

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
