{-# LANGUAGE FlexibleInstances #-}
module Sections.BackendGitAnnex 
    ( GitAnnexBackend(..)
    , mkGABE
    ) where

import Import
import Utils
import qualified Data.Text as T
import Data.Conduit
import Data.Conduit.Binary
import Data.Maybe
import Data.Conduit.Internal (Pipe(..), ConduitM(..))
import qualified Data.Conduit.Internal as CI
import Data.List hiding (insert, delete)
import qualified System.FilePath as FP
import qualified Data.Conduit.List as CL
import System.Process
import qualified Data.ByteString.Char8 as BC
import Database.Persist.Sql
import           Data.Time.Clock (UTCTime)

import Control.Monad.Trans.Maybe
import Sections.Types

import JSBrowser

import Debug.Trace

map3 :: (x -> a) -> (x -> b) -> (x -> c) -> x -> (a, b, c)
map3 f g h x = (f x, g x, h x)

data GitAnnexBackend = GitAnnexBackend
    { gaName :: Text
    , gaPath :: FilePath
    , gaRoute :: FPS -> Route App
    }

mkGABE :: SectionId -> MediaConf -> GitAnnexBackend
mkGABE section mc = GitAnnexBackend section (mcPath mc) (MediaContentR section)

instance ToJSON (MElem GitAnnexBackend) where
    toJSON = undefined

-- * Browse

instance MediaBrowsable App GitAnnexBackend where
    data MElem GitAnnexBackend = GAElem (Bool, FilePath, Maybe Text)

    browsableBanner    s = [whamlet|
<i .icon-white .icon-link>
    #{gaName s}
|]
    browsableJSRender    = undefined

    browsableFetchElems fps s = do
        pager <- liftHandlerT pagerSettings
        fetchElements s fps pager

    browsableRender source fps s = do
        pager <- liftHandlerT pagerSettings
        renderElements s pager fps source

renderElements :: GitAnnexBackend -> (Int, Int) -> FPS -> Source Handler (MElem GitAnnexBackend) -> Widget
renderElements s pager fps source = do
    elems <- liftHandlerT $ source $$ CL.consume
    case elems of
        (GAElem (True, path, desc) : []) -> renderSingle  s path desc
        xs                               -> renderListing s pager fps xs

renderSingle :: GitAnnexBackend -> FilePath -> Maybe Text -> Widget
renderSingle s path mdesc = do
    let section = gaName s
        fps     = FP.splitPath path
        hauto   = (MediaServeR ServeAuto          section fps, [])
        hforce  =  MediaServeR ServeForceDownload section fps
    simpleNav section fps (gaRoute s)
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

renderListing :: GitAnnexBackend -> (Int, Int) -> FPS -> [MElem GitAnnexBackend] -> Widget
renderListing s (limit, page) fps xs = do
    let sl = simpleListingSettings
                { slSect    = gaName s
                , slCurrent = fps
                , slCount   = length xs
                , slPage    = page
                , slLimit   = limit
                , slContent = map buildElem xs
                }
        buildElem (GAElem (isfile, path, _)) =
            ( T.pack path
            , if' isfile "file" "directory"
            , FP.splitPath path
            , "", "")
    simpleListing sl (gaRoute s)
                     (flip MediaServeR $ gaName s)
                     ("Filename", "File size", "Modified")

fetchElements :: GitAnnexBackend -> FPS -> (Int, Int) -> Source Handler (MElem GitAnnexBackend)
fetchElements s fps (per_page, current_page) = case fps of
    [] -> fetchRoot
    xs -> do
        md <- lift . runDB . getBy $ UniqueDNode (gaName s) path
        case md of
            Just dir -> fetchDirectory dir
            Nothing  -> lift (fetchFile path) >>= yield
  where
    section         = gaName s
    path            = FP.joinPath fps
    fetchRoot :: Source Handler (MElem GitAnnexBackend)
    fetchRoot       = runDBSource $ queryForRaw Nothing
    fetchDirectory  = runDBSource . queryForRaw . Just . entityKey

    fetchFile       = liftM (GAElem . map3 (const True) fNodePath fNodeDetails . entityVal)
        . runDB . getBy404 . UniqueFNode section

    queryForRaw Nothing = mapOutput toElem $ rawQuery (dirQuery True)
        [ toPersistValue section,  toPersistValue section
        , toPersistValue per_page, toPersistValue current_page ]

    queryForRaw parent  = mapOutput toElem $ rawQuery (dirQuery False)
        [ toPersistValue section,  toPersistValue parent -- dirs
        , toPersistValue section,  toPersistValue parent -- files
        , toPersistValue per_page, toPersistValue current_page ]

    dirQuery isNullParent = T.pack $ unlines
        [ "( SELECT FALSE as ts, path as paths, NULL FROM d_node"
        , "WHERE area = ? AND parent " <> if' isNullParent "IS NULL" "= ?"
        , ") UNION"
        , "( SELECT TRUE as ts, path as paths, details FROM f_node"
        , "WHERE area = ? AND parent " <> if' isNullParent "IS NULL" "= ?"
        , ") ORDER BY ts, paths LIMIT ? OFFSET ? " ]

toElem :: [PersistValue] -> MElem GitAnnexBackend
toElem [PersistBool isfile, PersistText path, PersistNull]      = GAElem (isfile, T.unpack path, Nothing)
toElem [PersistBool isfile, PersistText path, PersistText desc] = GAElem (isfile, T.unpack path, Just desc)
toElem _ = error "Sections.BackendGitAnnex.toElem: invalid value."

-- | Get pager settings.
pagerSettings :: Handler (Int, Int) -- TODO: move somewhere (browser? sections helpers?)
pagerSettings = do
    limit <- liftM (maybe 50 (read . T.unpack)) $ lookupGetParam "limit_to"
    page  <- liftM (maybe 0  (read . T.unpack)) $ lookupGetParam "page"
    return (limit, page)

-- * Search

instance MediaSearchable App GitAnnexBackend where
    searchableSearchT q s = searchFor s q

searchFor :: GitAnnexBackend -> Text -> Source Handler (MElem GitAnnexBackend)
searchFor s q = do
    (limit, offset) <- lift pagerSettings
    runDBSource $ mapOutput toElem $ rawQuery query
        [ toPersistValue section, toPersistValue $ ".*" <> q <> ".*"
        , toPersistValue section, toPersistValue $ ".*" <> q <> ".*"
        , toPersistValue limit, toPersistValue offset
        ]
  where
      section = gaName s
      query = T.pack $ unlines
        [ "( SELECT FALSE as ts, path as paths, NULL FROM d_node"
        , "WHERE area = ? AND path ~* ?"
        , ") UNION"
        , "( SELECT TRUE as ts, path as paths, details FROM f_node"
        , "WHERE area = ? AND path ~* ?"
        , ") ORDER BY ts, paths LIMIT ? OFFSET ? " ]


-- * Update

type UpdateTarget = Either FilePath FWrap

instance MediaUpdate App GitAnnexBackend where
  updateMedia ga = differenceSortedE unFWrap
        (dbSource (gaName ga)) (gitGetFileList (gaPath ga) "")
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
                      _      <- insert $ FNode (gaName ga) (entityKey <$> parent) path Nothing
                      pathToRecent path
                  FDir path -> do
                      parent <- findParent path
                      _      <- insert $ DNode (gaName ga) (entityKey <$> parent) path
                      pathToRecent path
              lift $ delete' todel -- reached a directory => delete queue (does this work always?)
              handler [] (ra' : ra)

      findParent     path = getBy $ UniqueDNode (gaName ga) (FP.takeDirectory path)
      delAll (Entity k _) = do deleteWhere [FNodeArea ==. gaName ga, FNodeParent ==. Just k]
                               delete k
      delete'          [] = return ()
      delete'       paths = runDB $ mapM_ delAll
          =<< selectList [DNodeArea ==. gaName ga, DNodePath <-. paths] [Asc DNodePath]

--pathToRecent :: 
pathToRecent path = return
    ( FP.splitPath path
    , toHtml $ last $ FP.splitPath path
    )
--addrecent time path = return $ RecentlyAdded
--    time (gaName ga) (map T.pack $ FP.splitPath path)
--    (toHtml $ last $ FP.splitPath path)
-- now <- timeNow

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

dbSource :: SectionId -> Source Handler FilePath
dbSource section = mapM_ yield =<< lift action
    where
        action :: Handler [FilePath]
        action = liftM (map unSingle) $ runDB $ rawSql query []
        query = T.pack $ unlines
            [ "SELECT path FROM f_node"
            , "UNION"
            , "SELECT path FROM d_node"
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

gitSource :: MonadIO m
          => Maybe FilePath -- ^ Path to repository
          -> [String] -> Source m FilePath
gitSource repo params =
    (git >>= sourceHandle) $= CL.mapFoldable (map BC.unpack . BC.lines)
  where
      git = liftIO $ liftM (\(_,h,_,_) -> h) $
          runInteractiveProcess "git" params repo Nothing

composeSubPaths :: [FilePath] -> [FilePath] -> [FWrap]
composeSubPaths xs ys = let
    paths = tail $ inits ys
    dirs  = init paths
    file  = last paths
    in map (FDir . FP.joinPath . (++) xs) dirs ++ [FFile $ FP.joinPath $ xs ++ file]
