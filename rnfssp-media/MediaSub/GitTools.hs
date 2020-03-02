{-# LANGUAGE NoImplicitPrelude, OverloadedStrings #-}
-- | Misc helpers for fetching info from git(-annex) repositories.
module MediaSub.GitTools where

import qualified Data.ByteString.Char8  as BC
import qualified Data.Conduit.List as CL
import           Data.Conduit.Binary
import           System.Process (runInteractiveProcess)

import           MediaSub.Import

type GitFile = (Bool, Text) -- directory, filename

-- | Read all files recursively.
sourceGitFiles :: MonadIO m
               => String -- ^ Path to repository
               -> FilePath -- ^ Directory to parse
               -> Source m GitFile
sourceGitFiles repo path = do
    xs <- sourceStdoutOf repo "sh" args $= sepOnNull $= toGitElem $$ CL.consume

    -- XXX: stupid sorting because of different locales here and in DB
    CL.sourceList $ sortBy (comparing snd) xs
  where
    args = ["-c", "git ls-tree $(git rev-parse --abbrev-ref HEAD) -rtz"]

toGitElem :: Monad m => Conduit Text m GitFile
toGitElem = CL.mapMaybe (f . words)
    where f :: [Text] -> Maybe (Bool, Text)
          f (_:t:_:xs) = Just ("t" `isPrefixOf` t, unwords xs)
          f         [] = Nothing
          f         xs = error $ "toGitElem: invalid input: " <> unpack (unwords xs)

-- | Conduit which concatenates bytestring chunks, splits the result on
-- null and converts parts to text (utf8).
sepOnNull :: Monad m => Conduit BC.ByteString m Text
sepOnNull = CL.concatMapAccum f BC.empty where
    f bs buffer = second (map decodeUtf8) $ case BC.split '\0' bs of
            []              -> (buffer,      []) -- (buffer, results)
            (x : [])        -> (buffer <> x, [])
            (x : xs@(_:_) ) -> (unsafeLast xs,     buffer <> x : unsafeInit xs)

sourceStdoutOf :: MonadIO m
               => String -- ^ Working directory
               -> String -- ^ Command
               -> [String] -- ^ Arguments
               -> Source m BC.ByteString
sourceStdoutOf wdir cmd params =
    cmd' >>= \(_,h,_,_) -> sourceHandle h where
        cmd' = liftIO $ runInteractiveProcess cmd params (Just wdir) Nothing
