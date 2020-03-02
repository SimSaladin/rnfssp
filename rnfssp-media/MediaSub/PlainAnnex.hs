{-# LANGUAGE NoImplicitPrelude, RankNTypes, TemplateHaskell, QuasiQuotes, OverloadedStrings, ConstraintKinds, FlexibleInstances, FlexibleContexts, TypeFamilies, GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module MediaSub.PlainAnnex where

import qualified Data.Conduit.List as CL
import           MediaSub.Import
import           MediaSub.Playlist
import           MediaSub.GitTools

plainAnnex :: YesodMediaSub m => FilePath -> SectionId -> Section m
plainAnnex gitdir sid = defaultSection
    { secGetPlayables = recursiveFiles gitdir
    , secGetContents = renderFiles sid . recursiveFiles gitdir
    }

renderFiles :: YesodMediaSub m => SectionId -> Source (HandlerT m IO) Text -> SubHandler m TypedContent
renderFiles sid source = selectRep $
    provideRep $ do
        elements <- lift $ source $$ CL.consume
        defaultLayoutMedia $
            [whamlet|

            |]

-- | Recursively find all files
recursiveFiles :: FilePath -> FPS -> Source (HandlerT m IO) Text
recursiveFiles gitdir fps =
        sourceGitFiles (fpToString gitdir) (fpsToFilePath fps)
        $= CL.mapMaybe playableOnly

playableOnly :: GitFile -> Maybe Text
playableOnly (False, path) = Just path
playableOnly _ = Nothing
