------------------------------------------------------------------------------
-- File:          JSBrowser.hs
-- Creation Date: Dec 18 2012 [02:04:15]
-- Last Modified: Dec 18 2012 [06:05:25]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module JSBrowser where

import Prelude
import Yesod
import Data.Text (Text)
import Data.List (init, last)
import qualified Data.Text as T
import Text.Julius (rawJS)
import qualified System.FilePath as F (joinPath)

-- | Javascript browser, which utilises the onpopstate functionality of modern
-- browsers. Assumptions this widget makes:
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
browser :: GWidget sub master ()
browser = do
  browserId <- lift newIdent
  toWidget [hamlet|
<div##{browserId}>
  |]
  toWidget [julius|
$(function(){
var dom       = $("##{rawJS browserId}"),
    last_href = location.href,
    ready     = false;

dom.load(location.href + "?bare=1", register_links);

window.history.replaceState('', '', location.href);

//$("##{rawJS browserId}").hide();
//$("##{rawJS browserId}").fadeIn(700);

function loadpage(href, update_history) {
  $.get(href + "?bare=1", function(data) {
    var from = (from == "") ? location.href : from,
        to   = (href.indexOf("http://") < 0)
          ?  location.href.substring(0, location.href.indexOf("/", 7)) + href
          : href;

    // remove trailing slashes.
    if (from.charAt(from.length - 1) == "/") { from = from.slice(0, -1); }
    if (to.charAt(to.length - 1) == "/")     { to   = to.slice(0, -1); }

    if (to == from) return;

    // push the new page to history, unless it was popped.
    if (update_history) { window.history.pushState('', '', href); }

    last_href = to;

//    // Transition params depend on whether we are going forward or backward in history.
//    if (to.split("/").length >= from.split("/").length) {
//      animOptOne = { direction : "left" };
//      animOptSec = { direction : "right" };
//    } else {
//      animOptOne = { direction : "right" };
//      animOptSec = { direction : "left" };
//    }
    dom.animate({ opacity: 0 }, 200, function() {
      dom.html(data);
      register_links();
      dom.animate({ opacity:1.0 }, 200);
    });
  });
}

/* Bind the function browser_load_page() to the browser links */
function register_links() {
  dom.find("a.browser-link").click(function() {
    loadpage($(this).attr("href"), true);
    return false;
  });
}

/* when asked to move in history */
window.onpopstate = function(e) {
  if (ready) loadpage(location.href, false);
  else browser_ready = true;
}

})
  |]

-- | Convert a widget to a whole page.
widgetToRepHtml :: Yesod master => GWidget sub master () -> GHandler sub master RepHtml
widgetToRepHtml w = do pc <- widgetToPageContent w
                       hamletToRepHtml [hamlet|^{pageBody pc}|]

-- | Simple listing content
simpleListing :: Text                               -- ^ Section
              -> [(Text, Route master)]             -- ^ Navigation
              -> [(Text, Text, [Text], Text, Text)] -- ^ (filename, filetype, fps, size, modified)
              -> ([Text] -> Route master)           -- ^ url to content
              -> (Text -> [Text] -> Route master)   -- ^ url to direct file
              -> (Text, Text, Text)                 -- ^ (msgFilename, msgFileize, msgModified)
              -> GWidget sub master ()
simpleListing section navParts listing toContent toFile (msgFilename, msgFileize, msgModified) = let
    in do 
  [whamlet|
<ul.breadcrum>
  $forall (name, route) <- init navParts
    <li>
      <a.browser-link href=@{route}>#{name}
      <span.divider>/
  $with (name, _) <- last navParts
    <li.active>#{name}
  <div.page-element.functional>
    <table#media-table .browser .tablesorter>
      <thead>
        <tr>
          <td.browser-controls scope="col">
          <td.browser-filename scope="col">#{msgFilename}
          <td.browser-size scope="col">#{msgFileize}
          <td.browser-modified scope="col">#{msgModified}

      <tbody>
        $forall (filename, filetype, fps, size, modified) <- listing
          <tr>
            <td.browser-controls>
              <a href="@{toContent fps}?to_playlist=1" onclick="to_playlist('#{section}', '#{toPath fps}'); return false">
                <i.icon-plus.icon-white>
              $if not $ equalsDirectory filetype
                <a href=@{toFile "force" fps} onclick="">
                  <i.icon-download-alt.icon-white>
                <a href=@{toFile "auto" fps} onclick="" target="_blank">
                  <i.icon-play.icon-white>

            <td.browser-filename>
              <a.browser-link.#{filetype} href=@{toContent fps}>
                <tt>#{filename}

            <td.browser-size>#{size}
            <td.browser-modified>#{modified}

  |]
    where
  toPath = T.pack . F.joinPath . map T.unpack
  equalsDirectory = (==) "directory"
