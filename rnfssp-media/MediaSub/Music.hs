{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE ConstraintKinds    #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE TypeFamilies       #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE QuasiQuotes        #-}

module MediaSub.Music (music) where

import           MediaSub.Import
import           MediaSub.Playlist
import           MediaSub.Browser
import           Network.MPD
import qualified Data.Conduit.List as CL
import qualified Data.List as L

import qualified Data.Text as T

type ExecMPD m = forall a. MPD a -> HandlerT m IO a

instance ToJSON LsResult where
    toJSON (LsDirectory (Path path)) = toJSON ("dir" :: Text, toJSON $ decodeUtf8 path)
    -- TODO implement the rest!

-- | A music section which exploits mpd for queries
music :: YesodMediaSub m => Port -> Host -> Password -> SectionId -> Section m
music port host pass = \sid -> defaultSection
    { secGetContents    = renderLsResult sid . musicLS exec
    , secGetPlayables   = filterPlayables . musicLS exec
    , secSearchWild     = musicSearch exec }
  where
    exec :: ExecMPD m
    exec = liftM onError . liftIO . withMPDEx host port pass

    onError (Left mpderror) = error $ "MPD Error: " <> show mpderror -- TODO not just error?
    onError (Right res)     = res

renderLsResult :: YesodMediaSub m => SectionId -> Source (HandlerT m IO) LsResult -> SubHandler m TypedContent
renderLsResult sid source =
    selectRep $ do
        provideRep $ liftM toJSON $ lift (source $$ CL.consume)
        provideRep $ do
            results <- lift $ source $$ CL.consume
            defaultLayoutMedia $ [whamlet|
$forall result <- results
  $case result
    $of LsDirectory path
      <div .media-directory>
        <a href=@{MediaContentR sid (pathToFPS path)} >#{pathToText path}

    $of LsSong song
      $with path <- sgFilePath song
        <div .media-element .media-song>
          <a href=@{MediaContentR sid (pathToFPS path)}>#{pathToText path}

    $of LsPlaylist playlist
      <div .media-playlist>#{playlistToText playlist}
|]

filterPlayables :: Source (HandlerT m IO) LsResult -> Source (HandlerT m IO) Text
filterPlayables = ($= CL.mapMaybe lsResultPlayable)

musicLS :: ExecMPD m -> FPS -> Source (HandlerT m IO) LsResult
musicLS exec fps =
    lift (liftHandlerT . exec . lsInfo $ fpsToPath fps) >>= CL.sourceList 

musicSearch :: ExecMPD m -> Text -> Source (HandlerT m IO) SearchResult
musicSearch exec q = do
    songs <- lift $ exec $ mpdSongSearch q
    CL.sourceList songs $= CL.map (SearchResult . const [])

lsResultPlayable :: LsResult -> Maybe Text
lsResultPlayable (LsSong song) = Just (pathToText $ sgFilePath song)
lsResultPlayable _             = Nothing

-- * MPD actions

-- | Find all songs with query string in artist, album or title.
mpdSongSearch :: Text -> MPD [Song]
mpdSongSearch q = doJoin `liftM` mapM doSearch [Artist, Album, Title]
  where
    doSearch val = search $ val =? Value (encodeUtf8 q)
    doJoin = take 100 . foldl' L.union []

pathToText :: Path -> Text
pathToText (Path path) = decodeUtf8 path

pathToFPS :: Path -> FPS
pathToFPS = T.splitOn "/" . pathToText

fpsToPath :: FPS -> Path
fpsToPath = Path . encodeUtf8 . fpsToText

playlistToText (PlaylistName pl) = decodeUtf8 pl
