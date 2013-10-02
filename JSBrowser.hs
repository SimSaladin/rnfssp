------------------------------------------------------------------------------
-- File:          JSBrowser.hs
-- Creation Date: Dec 18 2012 [02:04:15]
-- Last Modified: Oct 03 2013 [02:25:40]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | Javascript browser, which utilises the onpopstate functionality of modern
-- browsers.
--
--  How to use: *To be written*
module JSBrowser where

import           Prelude
import           Foundation
import           Yesod
import           Data.Text (Text)
import           Data.Monoid
import           Control.Applicative
import           Control.Arrow (second)
import qualified Data.Text as T
import           Text.Julius (rawJS)
import           Text.Coffee
import qualified System.FilePath as F (joinPath)

import           Utils
import Sections.Types

-- | The core widget in which the browser lives.
--
--  Assumptions:
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
browser :: WidgetT master IO ()
        -> [Text] -- ^ Optional doms that can bind browser content. (DomId, )
        -> WidgetT master IO ()
browser c extra = do
    bId <- liftHandlerT newIdent
    browserFrames bId c
    browserScript bId extra

-- | Markup
browserFrames :: Text -- ^ Browser ID
              -> WidgetT master IO () -- ^ Initial content
              -> WidgetT master IO ()
browserFrames browserId c = [whamlet|$newline never
<section .site-block-h>
    <h1>Browse
    <div ##{browserId}>
        ^{c}
|]

-- | 
browserScript :: Text   -- ^ Browser Dom ID
              -> [Text] -- ^ Extra Dom IDs
              -> WidgetT master IO ()
browserScript browserId extra = toWidget [coffee|
$ ->
  ## The browser dom. ###
  dom = $ "#%{rawJS browserId}"

  ### Bind a search form to be loaded by the browser. ###
  bind_form = (selector) ->
      $(selector).siblings("input").on "change",  -> $(selector).attr("disabled", null)
      $(selector).siblings("input").on "keydown", -> $(selector).attr("disabled", null)

      form = $(selector).closest("form")
      form.submit ->
          $(selector).attr("disabled", "")
          loadpage this.action + "?" + $(this).serialize(), true
          return false

  ### Bind all extra items - assuming they are all search forms :) ###
  bind_form s for s in %{rawJS $ show extra}

  ### Bind the function loadpage() to the browser links ###
  register_links = ->
      dom.find("a.browser-link").on "click", ->
          console.log encodeURI($(this).attr("href"))
          loadpage $(this).attr("href").replace("&", "%26"), true
          this.firstChild.focus()
          return false

  ### Load page href. Push the new page to history, unless it was popped. ###
  loadpage = (href, forward) ->
      $.get href, { bare:1 }, (data) ->
          window.history.pushState('', '', href) if forward and href != document.URL
          dom.animate { opacity: 0 }, 200, "ease", ->
              dom.html(data)
              register_links()
              dom.animate { opacity:1.0 }, 200

  ###
  window.onpopstate is called on page load at least in chromium but not in
  firefox. This is undesired. Fortunately on the page load event e.state is null
  in all browsers(?). So, we replace the current state to be non-null *before*
  binding the onpopstate event.
  ###
  window.history.replaceState {}, '', location.href
  window.onpopstate = (e) ->
      loadpage(location.href, false) if e.state isnt null

  ### Register links in the initial content. ###
  register_links()
|]

-- * Listing styles

-- ** Old simple

data SimpleListingSettings = SimpleListingSettings
    { slSect    :: SectionId    -- ^ Section
    , slCurrent :: FPS          -- ^ Url parts
    , slCount   :: Int          -- ^ Total number of elements to scroll
    , slPage    :: Int          -- ^ Nth page
    , slLimit   :: Int          -- ^ Elements per page
    , slContent :: [(Text, Text, FPS, Text, Text)] -- ^ (filename, filetype, fps, size, modified)
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
              -> (FPS -> Route master)                -- ^ Route to content.
              -> (ServeType -> FPS -> Route master)   -- ^ Route to file serving.
              -> (Text, Text, Text)                   -- ^ (msgFilename, msgFileize, msgModified)
              -> WidgetT master IO ()
simpleListing sl routeToContent toFile (_msgFilename, msgFilesize, msgModified) = do
    let options = [25, 50, 100, 200] :: [Int]
        parameters = if' (slLimit sl == 0) [] [ ("limit_to", T.pack $ show $ slLimit sl) ]

    --  Pager
    [whamlet|$newline never
<nav .text-center>
  Per page: #
  <span .btn-toolbar>
    $forall n <- options
      $if slLimit sl == n
        <a .btn .btn-small .disabled>#{n}
      $else
        <a .btn .btn-small .browser-link href="?limit_to=#{n}&page=#{slPage sl}">
            #{n} #
  <a .btn
    onclick="playlist.add_from_element_contents($('.browser .type-file .data-field'), '#{slSect sl}'); return false"
    title="Adds all files in this folder.">
      Add all files
^{simpleNav (slSect sl) (slCurrent sl) routeToContent}
^{pageNav}
<div .browser>
    $forall (filename, filetype, fps, size, modified) <- slContent sl
      <div .entry .type-#{filetype}>
        <div .data-field style="display:none">#{F.joinPath fps}
        <div .browser-controls>
          $if (/=) filetype directory
            <a .icon-download href=@{toFile ServeForceDownload fps} onclick="">
            <a .icon-play     href=@{toFile ServeAuto fps}          onclick="" target="_blank">
          <button .icon-plus .action onclick="playlist.to_playlist('#{slSect sl}', [$(this).closest('.entry').children()[0].innerText]); return false">
        <a .browser-link .#{filetype} href=@?{(routeToContent fps, parameters)}>
            <span .filename>#{filename}
            <span .misc>
                <span><i>#{msgModified}:</i> #{modified}
                $if (/=) filetype directory
                  <span>#{msgFilesize}: #{size}
    |]
    pageNav
        where
    directory   = "directory"
    pages       = [1 .. (ceiling $ (fromIntegral (slCount sl) :: Double) / fromIntegral (slLimit sl) :: Int)]
    pageNav     = if' (length pages == 1) mempty [whamlet|$newline never
<div .text-center .browser-pagenav>
  <span .btn-toolbar>
    $if slPage sl > 0
        <a .btn .btn-hl .btn-small .browser-link href=@{routeToContent $ slCurrent sl}?limit_to=#{slLimit sl}&page=#{slPage sl - 1}>Previous
    $else
        <a .btn .btn-small .disabled>Previous
    $forall n <- pages
        $if n == (slPage sl + 1)
            <a .btn .btn-small .disabled>#{n}
        $else
            <a .btn .btn-small .browser-link href=@{routeToContent $ slCurrent sl}?limit_to=#{slLimit sl}&page=#{n - 1}> #{n}
    $if slPage sl < length pages
        <a .btn .btn-hl .btn-small .browser-link href=@{routeToContent $ slCurrent sl}?limit_to=#{slLimit sl}&page=#{slPage sl + 1}>Next
    $else
        <a .btn .btn-small .disabled>Next
|]

-- ** Blocks

-- * Navigation

-- | Construct breadcrumbs into a widget.
simpleNav :: Text
          -> FPS
          -> (FPS -> Route master)
          -> WidgetT master IO ()
simpleNav _    []  _ = mempty
simpleNav home fps f = [whamlet|
<ul.breadcrumb>
    <li>
        <a .browser-link href=@{f mempty}>
            <i>#{home}
    <li .divider>/
  $forall (name, route) <- init parts
    <li>
      <a .browser-link href=@{route}>#{name}
    <li .divider>/
  $with (name, _) <- last parts
    <li.active>#{name}
|] where
  parts = map (second f) $ zip fps $ foldr (\x -> (:) [x] . map ([x] ++)) [[]] fps
