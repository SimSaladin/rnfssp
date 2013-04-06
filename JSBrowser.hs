------------------------------------------------------------------------------
-- File:          JSBrowser.hs
-- Creation Date: Dec 18 2012 [02:04:15]
-- Last Modified: Apr 05 2013 [23:54:18]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | Javascript browser, which utilises the onpopstate functionality of modern
-- browsers.
module JSBrowser where

import           Prelude
import   Foundation
import           Yesod
import           Data.Text (Text)
import           Control.Arrow (second)
import qualified Data.Text as T
import           Text.Julius (rawJS)
import           Text.Coffee
import qualified System.FilePath as F (joinPath)

-- | Assumptions this widget makes:
--
--  1. The current url must provide content to be shown inside the browser when
--  the GET parameter "bare" is set. The content can be either HTML (directly
--  injected to the browser) or JSON (converted to HTML via in-scope JS function
--  browser_content_to_html.
--
--  2. Before injecting, every <a class="browser-link> -element's click is bind to
--  loadpage, with "?bare=1" appended to href.
--
--  3. The loadpage() function again fetches the content, replaces links in it and
--  injects it to the browser.
--
--  NOTE: Only one browser instance may be safely based on a single page.
browser :: GWidget sub master () -- ^ Initial content of the browser.
        -> GWidget sub master ()
browser content = do
    browserId <- lift newIdent
    [whamlet|$newline never
<div##{browserId}>
    ^{content}
|]
    toWidget [coffee|
$ ->
    dom = $ "#%{rawJS browserId}"

    ### Bind the function loadpage() to the browser links ###
    register_links = ->
        dom.find("a.browser-link").on "click", ->
            loadpage $(this).attr("href"), true
            return false

    ### Load page href. Push the new page to history, unless it was popped. ###
    loadpage = (href, forward) ->
        $.get href + "?bare=1", (data) ->
            window.history.pushState('', '', href) if forward
            dom.animate { opacity: 0 }, 200, "ease", ->
                dom.html(data)
                register_links()
                dom.animate { opacity:1.0 }, 200

    ###
    window.onpopstate is called on page load at least in chromium but not in
    firefox. This is undesired, but on page load e.state is null. Then we also
    replace the current state to be non-null.
    ###
    window.history.replaceState {}, '', location.href
    window.onpopstate = (e) ->
        loadpage(location.href, false) if e.state isnt null

    ### Register links in the initial content ###
    register_links()
|]

data SimpleListingSettings = SimpleListingSettings
    { slSect    :: Text     -- ^ Section
    , slCurrent :: [Text]   -- ^ Url parts
    , slCount   :: Int      -- ^ Total number of elements to scroll
    , slPage    :: Int      -- ^ Nth page
    , slLimit   :: Int      -- ^ Elements per page
    , slContent :: [(Text, Text, [Text], Text, Text)] -- ^ (filename, filetype, fps, size, modified)
    }

simpleListingSettings :: SimpleListingSettings
simpleListingSettings = SimpleListingSettings
    { slSect    = ""
    , slCurrent = []
    , slCount   = 0
    , slPage    = 0
    , slLimit   = 50
    , slContent = []
    }

-- | Simple listing of content.
simpleListing :: SimpleListingSettings
              -> ([Text] -> Route master)                -- ^ url to content
              -> (ServeType -> [Text] -> Route master)   -- ^ url to direct file
              -> (Text, Text, Text)                      -- ^ (msgFilename, msgFileize, msgModified)
              -> GWidget sub master ()
simpleListing sl routeToContent toFile (msgFilename, msgFilesize, msgModified) = do
    simpleNav (slCurrent sl) routeToContent
    pageNav
    [whamlet|
<div .browser>
    $forall (filename, filetype, fps, size, modified) <- slContent sl
      <div .entry>
        <div .browser-controls>
            $if (/=) filetype directory
              <a .icon-download-alt .icon-white href=@{toFile ServeForceDownload fps} onclick="">
                  DL
              <a .icon-play .icon-white href=@{toFile ServeAuto fps} onclick="" target="_blank">
                  PLAY
            <a .icon-plus href="" onclick="playlist.to_playlist('#{slSect sl}', '#{toPath fps}'); return false">
                ADD
        <a .browser-link .#{filetype} href=@{routeToContent fps}>
            <span .filename>#{filename}
            <span .misc>
                <span><i>#{msgModified}:</i> #{modified}
                $if (/=) filetype directory
                    <span>#{msgFilesize}: #{size}
    |]
    pageNav
        where
    toPath      = T.pack . F.joinPath . map T.unpack -- FIXME ...
    directory   = "directory"
    file_other  = "file"
    pages       = [1 .. (ceiling $ (fromIntegral (slCount sl) :: Double) / fromIntegral (slLimit sl) :: Int)]
    pageNav     = [whamlet|
$if length pages > 1
    <div .top-nav>
        $if slPage sl > 0
            <a href=@{routeToContent $ slCurrent sl}?limit_to=#{slLimit sl}&page=#{slPage sl - 1}>Previous
        <span>
            $forall n <- pages
                <a href=@{routeToContent $ slCurrent sl}?limit_to=#{slLimit sl}&page=#{n - 1}> #{n}
        $if slPage sl < length pages
            <a href=@{routeToContent $ slCurrent sl}?limit_to=#{slLimit sl}&page=#{slPage sl + 1}>Next
|]

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
