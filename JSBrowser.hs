------------------------------------------------------------------------------
-- File:          JSBrowser.hs
-- Creation Date: Dec 18 2012 [02:04:15]
-- Last Modified: Dec 27 2012 [14:36:54]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | Javascript browser, which utilises the onpopstate functionality of modern
-- browsers.
module JSBrowser where

import           Prelude
import           Yesod
import           Data.Text (Text)
import           Control.Arrow (second)
import qualified Data.Text as T
import           Text.Julius (rawJS)
import qualified System.FilePath as F (joinPath)

-- | Assumptions this widget makes:
--
--  The current url must provide content to be shown inside the browser when
--  the GET parameter "bare" is set. The content can be either HTML (directly
--  injected to the browser) or JSON (converted to HTML via in-scope JS function
--  browser_content_to_html.
--
--  Before injecting, every <a class="browser-link> -element's click is bind to
--  loadpage, with "?bare=1" appended to href.
--
--  The loadpage function again fetches the content, replaces links in it and
--  injects it to the browser.
--
--  NOTE: Currently only one browser instance can be safely based on a single
--  site!
browser :: GWidget sub master ()
browser = do
  browserId <- lift newIdent
  toWidget [hamlet|<div##{browserId}>|]
  toWidget [julius|$(function(){
var dom       = $("##{rawJS browserId}"),
    previous  = location.href,
    ready     = false;

/* Bind the function loadpage() to the browser links */
function register_links() {
  debug("register_links");
  dom.find("a.browser-link").click(function() {
    loadpage($(this).attr("href"), true);
    return false;
  });
  ready = true;
}

function loadpage(href, push_history) {
  $.get(href + "?bare=1", function(data) {
    debug("loadpage callback");
    // push the new page to history, unless it was just popped.
    if (push_history && previous != href) {
      previous = href;
      window.history.pushState('', '', href);
    }
    dom.animate({ opacity: 0 }, 200, function() {
      dom.html(data);
      register_links();
      dom.animate({ opacity:1.0 }, 200);
    });
  });
}

/* when asked to move in history */
window.onpopstate = function(e) {
  debug("onpopstate");
  if (ready) loadpage(location.href, false);
}

// This calls window.onpopstate at least in chromium, but not in firefox.
window.history.replaceState('', '', location.href);

dom.load(location.href + "?bare=1", register_links);

function debug(where) {
  console.log("------> " + where + " <------");
  console.log("location.href: " + location.href);
  console.log("previous:      " + previous);
  console.log("ready:         " + ready);
}
})
  |]

-- | Simple listing content
simpleListing :: Text                               -- ^ Section
              -> [Text]                             -- ^ Url parts
              -> [(Text, Text, [Text], Text, Text)] -- ^ (filename, filetype, fps, size, modified)
              -> ([Text] -> Route master)           -- ^ url to content
              -> (Text -> [Text] -> Route master)   -- ^ url to direct file
              -> (Text, Text, Text)                 -- ^ (msgFilename, msgFileize, msgModified)
              -> GWidget sub master ()
simpleListing section fps listing routeToContent toFile (msgFilename, msgFileize, msgModified) =
  simpleNav fps routeToContent
  >> [whamlet|
<table .tablesorter .standout .browser>
  <thead>
    <tr>
      <th.browser-controls scope="col">
      <th.browser-filename scope="col">#{msgFilename}
      <th.browser-size scope="col">#{msgFileize}
      <th.browser-modified scope="col">#{msgModified}
  <tbody>
    $forall (filename, filetype, fps, size, modified) <- listing
      <tr>
        <td.browser-controls>
          <a .icon-plus .icon-white href="" onclick="playlist.to_playlist('#{section}', '#{toPath fps}'); return false">
          $if not $ equalsDirectory filetype
            <a .icon-download-alt .icon-white href=@{toFile "force" fps} onclick="">
            <a .icon-play .icon-white href=@{toFile "auto" fps} onclick="" target="_blank">
        <td.browser-filename title="#{filename}">
          <a.browser-link.#{filetype} href=@{routeToContent fps}>
            <tt>#{filename}
        <td.browser-size>#{size}
        <td.browser-modified>#{modified}
  <tfoot>
    <tr>
      <th.browser-controls scope="col">
      <th.browser-filename scope="col">#{msgFilename}
      <th.browser-size scope="col">#{msgFileize}
      <th.browser-modified scope="col">#{msgModified}

  |]
    where
  toPath = T.pack . F.joinPath . map T.unpack
  equalsDirectory = (==) "directory"

-- | Construct breadcrumbs into a widget.
simpleNav :: [Text] -> ([Text] -> Route master) -> GWidget sub master ()
simpleNav []  _ = [whamlet|<ul.breadcrumb>|]
simpleNav fps f = [whamlet|
<ul.breadcrumb>
  $forall (name, route) <- init parts
    <li>
      <a.browser-link href=@{route}>#{name}
      <span.divider>/
  $with (name, _) <- last parts
    <li.active>#{name}
|] where
  parts = map (second f) $ zip fps $ foldr (\x -> (:) [x] . map ([x] ++)) [[]] fps
