------------------------------------------------------------------------------
-- File:          Handler/Search.hs
-- Creation Date: Dec 16 2012 [22:12:57]
-- Last Modified: Dec 27 2012 [15:27:48]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module Handler.Search where

import Import
-- import Handler.Media
-- import Handler.Blog (
import qualified Text.Search.Sphinx as S
import qualified Text.Search.Sphinx as ST


data Result = RBlogPost { rId :: BlogpostId }
            | RDirectory
            | RFile

-- | run the search and return results in a page.
getSearchR :: Handler Html
getSearchR = do
  ((formRes, searchWidget),_) <- runFormGet searchForm
  searchResults <- case formRes of
    FormSuccess qstring -> getResults qstring
    _                   -> return []
  defaultLayout $ [whamlet|
whoops...
(results here?)
  |]

-- | Query sphinx and return results.
getResults :: Text -> Handler [Result]
getResults qstring = do
  sphinxRes' <- liftIO $ S.query config "searcher" qstring
  case sphinxRes' of
    ST.Ok sphinxRes -> do
      let dids = map (Key . PersistInt64 . ST.documentId) $ ST.matches sphinxRes
      fmap catMaybes $ runDB $ forM dids $ \docid -> do
        mdoc <- get docid
        case mdoc of
          Nothing -> return Nothing
          Just doc -> liftIO $ Just <$> getResult docid doc qstring
    _ -> error $ show sphinxRes'
  where
    config = S.defaultConfig
      { S.port = 9312
      , S.mode = ST.Any
      }


getResult :: *
