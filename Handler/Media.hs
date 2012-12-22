{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables, DoAndIfThenElse #-}
module Handler.Media
    ( getMediaHomeR
    , getMediaContentR
    , getMediaServeR
    , postMediaAdminR
    , getPlaylistR
    , postPlaylistR
    , adminWidget
    ) where

import           Utils
import           Import
import           JSBrowser
import           Control.Arrow ((***))
import           Control.Monad
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import           Data.List (head, tail)
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Time.Clock (diffUTCTime, NominalDiffTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format (formatTime)
import           System.Directory (getTemporaryDirectory)
import           System.FilePath ((</>), normalise)
import           System.IO (hClose)
import           System.IO.Temp (openTempFile)
import           System.Posix.Files (FileStatus, fileSize, modificationTime, isDirectory)
import           System.Process (readProcessWithExitCode)
import qualified System.FilePath.Find as F
import           System.Locale (defaultTimeLocale)

-- | Maximum time a file can be accessed via a temporary url.
maxTempUrlAlive :: NominalDiffTime
maxTempUrlAlive = 60 * 60 * 24

browsable :: [(Text, Text)]
browsable = [ ("anime", "film"), ("music", "music")]

videoExtensions :: [FilePath]
videoExtensions = [".mkv", ".avi", ".jpg"]


-- * General / Content

getMediaHomeR :: Handler RepHtml
getMediaHomeR = do
  mauth <- maybeAuth
  defaultLayout $ do
    setTitle "Media"
    $(widgetFile "media-home")
  where sections = mediaSectionNavs ""

mediaSectionNavs :: Text -> [(Bool, Route App, Text, Text)]
mediaSectionNavs current = map (\(s, i) -> (current == s, MediaContentR s [], s, i)) browsable

getMediaContentR :: Text -> [Text] -> Handler RepHtml
getMediaContentR section fps = do
  mauth <- maybeAuth
  bare <- lookupGetParam "bare"
  case bare of
      Just  _ -> widgetToRepHtml $ contentWidget section fps
      Nothing -> defaultLayout $ do
          setTitle "Media"
          $(widgetFile "media-content")
    where sections = mediaSectionNavs section

restrictedWidget :: Widget
restrictedWidget = [whamlet|
<div.hero-unit.center-box.small-box>
  <h1>Access restricted
  <p>
    You must be explicitly granted access to this part.<br/>
    Please, #
    <a.btn.btn-primary href="@{routeToLogin}">Login
    &nbsp;or #
    <a.btn.btn-info href="@{RegisterR}">Register
    .
|]

-- | Generate content based on section and path.
contentWidget :: Text -> [Text] -> Widget
contentWidget "anime" fps = animeContent fps -- undefined -- listing from db
contentWidget "music" fps = undefined -- mpd stuff here
contentWidget       _   _ = lift notFound

-- | Convert anime path to content. Normal listing on directories, player in
--   files.
animeContent :: [Text] -> Widget
animeContent fps = do
  (mnode, mchildren) <- lift $ runDB $ do
    case fps of
      [] -> liftM ((,) Nothing . Just) $ selectList [FilenodeParent ==. Nothing] [Asc FilenodePath]
      fps' -> do
        Entity key val <- getBy404 $ UniqueFilenode "anime" (T.intercalate "/" fps') -- FIXME; '/' as path separator
        liftM ((,) $ Just $ Entity key val) $ if filenodeIsdir val
          then liftM Just $ selectList [FilenodeParent ==. (Just key)] [Asc FilenodePath]
          else return Nothing

  case (mnode, mchildren) of
    (_, Just children) -> simpleListing "anime"
                                   (constructAnimeNav fps)
                                   (map (buildElem . entityVal) children)
                                   (MediaContentR "anime")
                                   (flip MediaServeR "anime")
                                   ("Filename", "File size", "Modified")
    (Just node, _) -> animeSingle fps node
    _ -> lift notFound
    where
  buildElem val = ( filenodePath val
                  , if' (filenodeIsdir val) "directory" "file"
                  , T.splitOn "/" $ filenodePath val
                  , filenodeSize val
                  , T.pack $ formatTime defaultTimeLocale "%d.%m -%y" $ filenodeModTime val
                  )

-- |
constructAnimeNav :: [Text] -> [(Text, Route App)]
constructAnimeNav fps = map (id *** MediaContentR "anime") $ ("anime", []) : zip fps (foldr (\x xs -> [x] : map ([x] ++) xs) [[]] fps)

-- |
animeSingle :: [Text] -> Entity Filenode -> Widget
animeSingle fps (Entity _ val) = do
  simpleNav $ constructAnimeNav fps
  [whamlet|
<div .container-fluid>
  <div..row-fluid>
    <div.span7>
      <table>
        <tr>
          <th>Filename
          <td><i>#{filenodePath val}
        <tr>
          <th>Size
          <td>#{filenodeSize val}
        <tr>
          <th>Modified
          <td>#{T.pack $ formatTime defaultTimeLocale "%d.%m -%y" $ filenodeModTime val}
    <div.span5>
      <a.btn.btn-primary href="@{MediaServeR "auto" "anime" fps}" target="_blank">
        <i.icon-white.icon-play> #
        Auto-open
      <a.btn href="@{MediaServeR "force" "anime" fps}">
        <i.icon.icon-download-alt> #
        Download
      <a.btn onclick="window.playlist.to_playlist('anime', '#{toPath fps}'); return false">
        Add to playlist
  $maybe s <- filenodeDetails val
    <h4>Mediainfo
    <pre>#{s}
  |]

-- * Playing / Downloading files

-- | Downloading files
getMediaServeR :: Text   -- ^ kind of download
               -> Text   -- ^ file section
               -> [Text] -- ^ file path
               -> Handler RepJson
getMediaServeR kind section path
  | null path       = invalidArgs ["Invalid file."]
  | kind == "temp"  = solveTemp >>= send ""
  | kind == "auto"  = solvePathWithAuth section path >>= send ""
  | kind == "force" = solvePathWithAuth section path >>= send "application/force-download"
  | otherwise       = invalidArgs ["Invalid or unsupported download type."]
    where
  send ct fp = setHeader "Accept-Ranges" "bytes" >> sendFile ct fp
  solveTemp  = do
    Entity _ (DlTemp time _ target) <- runDB $ getBy404 $ UniqueDlTemp $ head path
    now <- timeNow
    denyIf (diffUTCTime now time > maxTempUrlAlive) "File not available."
    denyIf (target /= toPath (tail path)          ) "Malformed url."
    toFSPath section $ T.unpack target

-- | Solve section+path down to a FilePath. 404 if user is not authenticated.
-- Logs a successful download.
solvePathWithAuth :: Text -> [Text] -> Handler FilePath
solvePathWithAuth section path = do
    uid <- requireAuthId
    Entity key val <- runDB $ getBy404 $ UniqueFilenode section (toPath path)
    if filenodeIsdir val
      then invalidArgs ["Downloading directories is not supported."]
      else do t <- timeNow
              _ <- runDB $ insert $ LogDownload uid t key
              toFSPath section $ T.unpack $ filenodePath val


-- * Playlists

-- | Get the active playlist with @solvePlaylist@ and send it as m3u.
getPlaylistR :: Text -> Handler RepPlain
getPlaylistR action
    | action `T.isPrefixOf` "get"  = requireAuth
      >>= solvePlaylist
      >>= generateM3U . entityVal
      >>= if "-force" `T.isSuffixOf` action
        then \x -> do setHeader "Content-Disposition" "attachment; filename=\"playlist.m3u\""
                      sendFile "application/force-download" x
        else sendFile "audio/x-mpegurl"
    | otherwise = invalidArgs ["Invalid or unsupported action: " `T.append` action]

-- | Perform actions on playlists. action is the first part in the uri.
--
-- action is one of:
--    push:   adds an filenode and its children to current playlist, and
--            save the playlist.
--
--    clear:  remove all elements from the playlist.
--
postPlaylistR :: Text -> Handler RepJson
postPlaylistR action = do
    Entity plk pl <- solvePlaylist =<< requireAuth
    case action of
      "select" -> rsucc pl
      "insert" -> do
          (section, what) <- parseJsonBody_
          (new, pl') <- toPlaylist pl section what
          if new
            then updatePlaylist plk pl' >>= rsucc
            else rfail "no such path"
      "delete"  -> updatePlaylist plk (clearPlaylist pl) >>= rsucc
      _ -> rfail "unknown playlist action"
  where
    rfail :: Text -> Handler RepJson
    rfail msg = jsonToRepJson (1 :: Int, msg)
    rsucc msg = jsonToRepJson (0 :: Int, msg)

-- | user specific playlist widget
playlist :: Entity User -> Widget
playlist (Entity _ uval) = do
    [main, actions, content, heading] <- replicateM 4 (lift newIdent)
    $(widgetFile "media-playlist")

-- | Retrieve user's playlist using various methods.
-- order: getparam -> cookie -> database -> new playlist
--
-- if "title" get parameter is set and no playlist is found, result is 404.
-- note: playlist with a title of "" is the default for any user
--
-- XXX: support hidden playlists?
solvePlaylist :: Entity User -> Handler (Entity Playlist)
solvePlaylist (Entity k v) = fromGetparam
  where
    fromGetparam = lookupGetParam "title" >>= \mt -> case mt of
        Just title -> runDB (getBy (UniquePlaylist k title)) >>= tryMaybe fromCookie 
        Nothing    -> fromCookie

    fromCookie = lookupCookie "playlist-title" >>= \mt' -> case mt' of
        Just title -> runDB (getBy (UniquePlaylist k title)) >>= tryMaybe fromDB
        Nothing    -> fromDB

    fromDB = case userCurrentplaylist v of
        Just name -> runDB (getBy $ UniquePlaylist k name) >>= tryMaybe fromDBDefault
        Nothing   -> fromDBDefault

    fromDBDefault = runDB (getBy $ UniquePlaylist k "") >>= tryMaybe (addDefaultPlaylist k)

-- | Generate a m3u-formatted file of the playlist with random-generated
-- check-texts for access from any(?) client.
-- Saves to a temporary file.
-- File format:
--
-- > #EXTM3U
-- > #EXTINF:length, extra_info
-- > @{MediaServeR "temp" hash path}
-- > ...
--
generateM3U :: Playlist -> Handler FilePath
generateM3U pl = do
    resolved <- gServeroot
    yesod    <- getYesod
    time     <- timeNow
    let render a p t = yesodRender
          yesod resolved (MediaServeR "temp" a (t : splitPath' p)) []

        write h (area, path) = do
            temp <- randomText 32
            T.hPutStrLn h (render area path temp)
            return (temp, path)

    (path, temps) <- liftIO $ do
      (path, handle) <- getTemporaryDirectory >>= flip openTempFile "playlist.m3u"
      T.hPutStrLn handle "#EXTM3U\n"
      temps <- mapM (write handle) $ playlistElems pl
      hClose handle
      return (path, temps)
    mapM_ (uncurry $ ((runDB . insert) .) . DlTemp time) temps
    return path


-- ** Playlist actions

clearPlaylist :: Playlist -> Playlist
clearPlaylist pl = pl { playlistElems = [] }

-- | add file OR every child of directory to playlist.
toPlaylist :: Playlist
           -> Text -- ^ File area
           -> Text -- ^ File path
           -> Handler (Bool, Playlist) -- (playlist changed?, changed playlist)
toPlaylist pl section path = do
    t <- timeNow
    new <- findFilenodeFiles section path
    return $ case new of
      [] -> (False, pl)
      new -> (True, pl{ playlistElems = playlistElems pl ++ map ((,) section) new
                     , playlistModified = t 
                     })

-- | New titleless playlist for user with userId uid.
-- XXX: doesn't check for duplicates(!)
addDefaultPlaylist :: UserId -> Handler (Entity Playlist)
addDefaultPlaylist uid = do
    t <- timeNow
    let pl = Playlist "" uid [] t t
    k <- runDB $ insert pl
    return $ Entity k pl

-- | saves (replaces) an existing playlist. This is unsafe (replace) due to
-- unique constraints.
updatePlaylist :: PlaylistId -- ^ id of playlist to replace
               -> Playlist
               -> Handler Playlist -- ^ was update successful?
updatePlaylist plid pl = runDB (replace plid pl) >> return pl

-- | Get all user's playlists.
myPlaylists :: Handler [Entity Playlist]
myPlaylists = runDB $ selectList ([] :: [Filter Playlist]) []

-- | Recursively find all child files (and only files) of a node.
findFilenodeFiles :: Text -> Text -> Handler [Text]
findFilenodeFiles area path = do
    Entity k v <- runDB $ getBy404 $ UniqueFilenode area path
    if filenodeIsdir v
    then do
      children <- liftM (map (filenodePath . entityVal)) $ runDB $
                  selectList [FilenodeParent ==. Just k] [Asc FilenodePath]
      liftM concat $ mapM (findFilenodeFiles area) children
    else return [filenodePath v]


-- * Adminspace

-- | A widget for media administration functionality, to be embebbed in the
--   centralised admin page.
adminWidget :: Widget
adminWidget = do
  ((result, widget), encType) <- lift $ runFormPost adminForm
  [whamlet|
<h1>Media
<div>
  <form.form-horizontal method=post action=@{MediaAdminR} enctype=#{encType}>
    <fieldset>
      <legend>Media actions
      $case result
        $of FormFailure reasons
          $forall reason <- reasons
            <div .alert .alert-error>#{reason}
        $of _
      ^{widget}
      <div .form-actions>
        <input .btn .primary type=submit value="Execute actions">
  |]

-- | Admin operations in Media.
postMediaAdminR :: Handler RepHtml
postMediaAdminR = do
   path <- gdir "anime"
   updateListing "anime" path
   redirect AdminR

adminForm :: Form (Bool, Bool)
adminForm = renderBootstrap $ (,)
   <$> areq boolField "Update all anime" (Just False)
   <*> areq boolField "Not used" (Just False)

-- | 
updateListing :: Text
              -> FilePath
              -> Handler ()
updateListing section dir = do
  -- recursively find files and directories in `dir` along with properties
  let filterp = foldl (F.||?) (F.fileType F.==? F.Directory F.&&? F.filePath F./=? dir)
                      $ map (F.extension F.==?) videoExtensions

  -- foldl (F.||?)  $ map (F.extension F.==?) videoExtensions
      makeRel = dropWhile (== '/') . drop (length dir)

  (filesInFS, filesInFS') <- liftIO $ F.fold
      F.always
      (\(list, set) fi -> if F.evalClause filterp fi
          then let tpath = T.pack $ makeRel $ F.infoPath fi
            in (list ++ [(tpath, F.infoStatus fi)], S.insert tpath set)
          else (list, set))
      ([], S.empty)
      dir

  let fileNotInFS = flip S.notMember filesInFS' . filenodePath . entityVal

  liftIO $ print "--------------------------- filesInFS --------------------" >> print (show $ map fst filesInFS) >> print "--------------------------- END -------------------------"

  -- delete files not found in the filesystem
  -- XXX: disable/hide only?
  deleted <- runDB $ C.runResourceT $ selectSource [FilenodeArea ==. section] [Desc FilenodePath]
      C.$= CL.filter fileNotInFS
      C.$= CL.map entityKey C.$= CL.mapM delete
      C.$$ CL.consume -- do something with results?

  liftIO $ print "--------------------------- filesInFS' --------------------" >> print (show filesInFS') >> print "--------------------------- END -------------------------"

  -- find completely new entities and insert them and their info
  added <- runDB $ C.runResourceT $ (CL.sourceList filesInFS)
    C.$= CL.mapMaybeM (\x -> liftM (flip ifNothing x) $ getBy $ UniqueFilenode section $ fst x)
    C.$= CL.mapM insertNode
    C.$$ CL.consume

  liftIO $ print "---------------------------- files added -------------------" >> print added >> print "--------------------------- END -------------------------"

  -- TODO: find and update modified (newer than db) entities?

  -- Try to add parents to nodes. XXX: This is needed becouse..?
  fixed <- runDB $ C.runResourceT $ selectSource [FilenodeArea ==. section, FilenodeParent ==. Nothing] []
      C.$= CL.mapM fixParent
      C.$$ CL.consume

  liftIO $ print "---------------------------- files fixed -------------------" >> print fixed >> print "--------------------------- END -------------------------"

  return ()
    where
  insertNode (path,stat) = do
      parent <- getBy $ UniqueFilenode section (takeDirectory' path)
      insert =<< liftIO (toFilenode (dir </> T.unpack path)
                                    section
                                    (entityKey <$> parent)
                                    (T.pack $ normalise $ T.unpack path)
                                    stat)

  fixParent (Entity key val) = do
      parent <- getBy $ UniqueFilenode section (takeDirectory' $ filenodePath val)
      update key [FilenodeParent =. fmap entityKey parent]

  ifNothing mx x = case mx of
      Nothing -> Just x
      Just _  -> Nothing

-- | Generate a filenode from properties.
toFilenode :: FilePath         -- ^ Real path to the file
           -> Text             -- ^ area
           -> Maybe FilenodeId -- ^ parent node
           -> Text             -- ^ path of the node
           -> FileStatus       -- ^ status of the node
           -> IO Filenode
toFilenode real section parent path stat = do
    details <- if' isdir (return Nothing) $ Just <$> getDetails real
    return $ Filenode section parent isdir path
                      (prettyFilesize $ fileSize stat)
                      (posixSecondsToUTCTime $ realToFrac $ modificationTime stat)
                      details
    where isdir = isDirectory stat

-- |
getDetails :: FilePath -> IO Text
getDetails fp = do
  (_, out, _) <- readProcessWithExitCode "mediainfo" [fp] ""
  return $ T.pack out

