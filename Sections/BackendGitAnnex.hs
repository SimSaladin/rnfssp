module Sections.BackendGitAnnex where

import Sections
import Import
import Utils
import qualified Data.Text as T
import Data.Conduit
import Data.Conduit.Binary
import Data.Maybe
import Data.Conduit.Internal (Pipe(..), ConduitM(..))
import qualified Data.Conduit.Internal as CI
import Data.List hiding (insert)
import qualified System.FilePath as FP
import qualified Data.Conduit.List as CL
import System.Process
import qualified Data.ByteString.Char8 as BC
import Database.Persist.Sql
import Debug.Trace
import           Data.Time.Clock (UTCTime)

import Control.Monad.Trans.Maybe
import Sections.Types

data GitAnnexBackend = GitAnnexBackend
    { gaName :: Text
    , gaPath :: FilePath
    , gaRoute :: [Text] -> Route App
    }

mkGABE :: Section -> MediaConf -> GitAnnexBackend
mkGABE section mc = GitAnnexBackend section (mcPath mc) (MediaContentR section)

instance MSection GitAnnexBackend where
    sWContent = getContent
    sUpdateIndex = updateIndex

instance MediaBrowsable master GitAnnexBackend where
    browsableContent     = undefined

    browsableRenderer    = undefined

    browsableDescription = undefined

    browsableFindElems   = undefined

-- * Content

getContent :: GitAnnexBackend -> [Text] -> Widget
getContent ga fps = do
    undefined

-- * Update

type UpdateTarget = Either FilePath FWrap

updateIndex :: GitAnnexBackend -> Handler [RecentlyAdded]
updateIndex ga = do
    now <- timeNow
    differenceSortedE unFWrap (dbSource (gaName ga)) (gitGetFileList (gaPath ga) "")
        $$ handler now [] []
    where
      delete'          [] = return ()
      delete'       paths = runDB $ deleteWhere [DNodeArea ==. gaName ga, DNodePath <-. paths]
      findParent     path = getBy $ UniqueDNode (gaName ga) (FP.takeDirectory path)
      addrecent time path = return $ RecentlyAdded time (gaName ga) (map T.pack $ FP.splitPath path)
                                                                 (toHtml $ last $ FP.splitPath path)
      handler :: UTCTime -> [FilePath] -> [RecentlyAdded] -> Sink UpdateTarget Handler [RecentlyAdded]
      handler now deleteThese ra = await >>= maybe (lift (delete' deleteThese) >> return ra) handleElement
        where
            handleElement (Left fp) = handler now (deleteThese ++ [fp]) ra
            handleElement (Right e) = do
              ra' <- lift $ runDB $ case e of
                  FFile path -> do
                      parent <- findParent
                      _      <- insert $ FNode (gaName ga) (entityKey <$> parent) path Nothing
                      addrecent now path
                  FDir path -> do
                      parent <- findParent
                      _      <- insert $ DNode (gaName ga) (entityKey <$> parent) path
                      addrecent now path
              lift $ delete' deleteThese
              handler now [] (ra':ra)

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

dbSource :: Section -> Source Handler FilePath
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
