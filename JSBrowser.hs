------------------------------------------------------------------------------
-- File:          JSBrowser.hs
-- Creation Date: Dec 17 2012 [19:08:11]
-- Last Modified: Dec 17 2012 [22:11:35]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module JSBrowser
  ( browserWidget
  , browserBare
  ) where

import Import
import Utils
import Data.List (head, last, tail, init)
import           Data.Time.Format (formatTime)
import qualified Data.Text as T
import           System.FilePath (takeFileName, normalise)
import qualified System.FilePath as F (joinPath)
import           System.Locale (defaultTimeLocale)

toPath :: [Text] -> Text
toPath = T.pack . F.joinPath . map T.unpack

equalsDirectory :: Text -> Bool
equalsDirectory = (==) "directory"

-- | Browser widget.
browserWidget :: [Text] -- ^ Path to initialize the widget in.
              -> Widget
browserWidget fps = do
    browserId <- lift newIdent
    $(widgetFile "browser-driver")
  where
    sections = map (\(sect, icon) -> (current == sect, MediaR [sect], sect, icon)) browsable
    current  = if' (null fps) "" (head fps)

-- | Bare <table> element only of the browser.
browserBare :: [Text] -> Handler RepHtml
browserBare fps = do
    _ <- requireAuth
    pc <- widgetToPageContent $ browserViewWidget fps
    hamletToRepHtml [hamlet|^{pageBody pc}|]

-- | The view of a path, which is either a listing or a file.
--  NOTE: Requires the browser script (from browserWidget) on the page.
browserViewWidget :: [Text] -> Widget
browserViewWidget [] = let
    nav     = [] :: [(Text,Texts)]
    area    = "" :: Text
    is_dir  = False
    is_file = False
    details = Nothing :: Maybe String
    listing = [] :: [(String, Text, [Text], [Text], Text, Text)]
    in $(widgetFile "browser-bare")
browserViewWidget (area:path) = do
    node <- lift $ runDB $ getBy404 $ UniqueFilenode area (T.pack $ normalise $ F.joinPath $ map T.unpack path)

    let val     = entityVal node
        is_dir  = filenodeIsdir val
        is_file = not is_dir
        details = filenodeDetails val

    nodes <- if is_file
      then return []
      else lift $ runDB $ selectList [ FilenodeParent ==. (Just $ entityKey node)
                                     , FilenodePath   !=. "." ] [Asc FilenodePath]

    let listing = do
        this <- map entityVal nodes
        let file     = takeFileName $ T.unpack $ filenodePath this
            filetype = if filenodeIsdir this
                         then "directory"
                         else guessFiletype file
            fps      = path ++ [T.pack file]
            size     = filenodeSize this
            modified = formatTime defaultTimeLocale "%d.%m -%y" $ filenodeModTime this
--            details  = filenodeDetails this
            in return (file, filetype, fps, area : fps, size, modified)
    $(widgetFile "browser-bare")
  where
    nav = zip (area:path) (foldr (\x xs -> [x] : map ([x] ++) xs) [[]] (area:path))

-- | ...
browsable :: [(Text, Text)]
browsable = [ ("anime", "film"), ("music", "music")]

