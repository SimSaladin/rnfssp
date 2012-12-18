------------------------------------------------------------------------------
-- File:          JSBrowser.hs
-- Creation Date: Dec 18 2012 [02:04:15]
-- Last Modified: Dec 18 2012 [04:13:37]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------
module JSBrowser where

import Yesod
import Text.Julius (rawJS)

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
$(
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
)

/* Bind the function browser_load_page() to the browser links */
function register_links() {
  dom.find("a.browser-link").click(function() {
    loadpage($(this).attr("href"), true);
    return false;
  });
}

/* when asked to move in history */
window.onpopstate = function(e) {
  if (ready)
    loadpage(location.href, false);
  else
    browser_ready = true;
}

  |]

-- | Convert a widget to a whole page.
widgetToRepHtml :: Yesod master => GWidget sub master () -> GHandler sub master RepHtml
widgetToRepHtml w = do pc <- widgetToPageContent w
                       hamletToRepHtml [hamlet|^{pageBody pc}|]
