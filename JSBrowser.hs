{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
------------------------------------------------------------------------------
-- File:          JSBrowser.hs
-- Creation Date: Dec 18 2012 [02:04:15]
-- Last Modified: Oct 09 2013 [05:15:14]
-- Created By: Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
------------------------------------------------------------------------------

-- | Javascript browser, which utilises the onpopstate functionality of modern
-- browsers.
--
--  How to use: *To be written*
module JSBrowser where

import           Prelude
import           Foundation (ServeType(..))
import           Yesod
import           Data.Text (Text)
import           Data.Monoid
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Control.Applicative
import           Control.Monad
import           Control.Arrow (second)
import qualified Data.Text as T
import           Text.Julius (rawJS)
import           Text.Coffee
import qualified System.FilePath as F (joinPath)

import           Utils
import Sections.Types

-- * Browser
 
data BrowserSettings master = BrowserSettings
    { browserId         :: Text    -- ^ Identifier for the browser ("mybrowser")
    , browserLinksMatch :: Text    -- ^ (JQuery) matcher for browser links, e.g. "a.browser-link"
    , browserExtraDoms  :: [Text]  -- ^ Optional doms that can bind browser content. (DomId, )
    , browserContent    :: WidgetT master IO () -- ^ Initial/fallback content widget
    }

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
browser :: BrowserSettings master -> WidgetT master IO ()
browser = (>>) <$> browserFrames <*> browserScript

-- | Markup
browserFrames :: BrowserSettings master -> WidgetT master IO ()
browserFrames config = [whamlet|$newline never
<section .site-block-h>
    <h1>Browse
    <div ##{browserId config}>
        ^{browserContent config}
|]

-- | 
browserScript :: BrowserSettings master -> WidgetT master IO ()
browserScript config = toWidget [coffee|
$ ->
  ## The browser dom. ###
  dom = $ "#%{rawJS $ browserId config}"

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
  bind_form s for s in %{rawJS $ show $ browserExtraDoms config}

  ### Bind the function loadpage() to the browser links ###
  register_links = ->
      dom.find("%{rawJS $ browserLinksMatch config}").on "click", ->
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

-- * Rendering

renderDefault :: MediaRenderDefault master section
              => SectionId -> FPS 
              -> ListContent master section -- ^ Content
              -> (             SectionId -> FPS -> Route master) -- ^ ContentR
              -> (ServeType -> SectionId -> FPS -> Route master) -- ^ ServeR
              -> WidgetT master IO ()
renderDefault secid fps (ListSingle _huh) contentR serveR = [whamlet|
<section>
    ^{simpleBreadcrumbs secid fps (contentR secid)}
    <h1>#{F.joinPath fps}
    ^{mediaSingleControls secid fps serveR}
|]
-- many: just flat listing
renderDefault secid fps (ListMany source (ListFlat paging numof)) contentR serveR = listFlat
    $ FlatListSettings (contentR secid) (`serveR` secid)
                       secid fps paging
                       (maybe 1 (\x -> ceiling $ (fromIntegral x :: Double) / fromIntegral (fst paging)) numof)
                       source

mediaSingleControls :: SectionId -> FPS
                    -> (ServeType -> SectionId -> FPS -> Route master) -- ^ ServeR
                    -> WidgetT master IO ()
mediaSingleControls secid fps serveR = [whamlet|
<div .text-center>
    <div .btn-group>
      <a .btn .btn-primary href="@?{hauto}" target="_blank"><i .icon-white .icon-play></i>
          Auto-open
      <a .btn href="@{hforce}"><i .icon .icon-download-alt></i>
          Download
      <a .btn onclick="window.playlist.to_playlist('#{secid}', ['#{F.joinPath fps}']); return false">
          To playlist
|] where hauto  = (serveR ServeAuto          secid fps, [])
         hforce =  serveR ServeForceDownload secid fps

-- * Listing styles

-- ** Flat

data FlatListSettings master section = FlatListSettings
    { lfViewR       ::              FPS -> Route master
    , lfServeR      :: ServeType -> FPS -> Route master
    , lfSec         :: SectionId
    , lfCurrentFPS  :: FPS
    , lfPaging      :: Paging
    , lfNumPages    :: Int
    , lfContent     :: MediaSource master (FPS, MElem master section)
    }

listFlat :: MediaRenderDefault master section
         => FlatListSettings master section -> WidgetT master IO ()
listFlat config = do
    [whamlet|
^{brwidget}
<div .clearfix .hl-on-hover style="width:100%">
    <div .float-left>^{browseSettings}
    <div .float-right>^{playlistAddAll}
    |]
    elements <- liftHandlerT $ mapOutput (second melemToContent) (lfContent config) $$ CL.consume
    -- TODO data-field to a custom data-path attribute of .entry.
    [whamlet|$newline never
<div .browser>
  <div .text-center .hl-on-hover>^{browseNavigation}
  $forall (fps, (ftype, xs)) <- elements
    <div .entry .type-#{ftype}>
        <div .data-field style="display:none">#{F.joinPath fps}
        <div .browser-controls>
          $if (/=) ftype directory
            <a .icon-download href=@{(lfServeR config) ServeForceDownload fps} onclick="">
            <a .icon-play     href=@{(lfServeR config) ServeAuto fps}          onclick="" target="_blank">
          <button .icon-plus .action onclick="playlist.to_playlist('#{lfSec config}', [$(this).closest('.entry').children()[0].innerText]); return false">
        <a .browser-link .#{ftype} href=@?{ ( (lfViewR config) fps, parameters )}>
            <span .filename>#{last fps}
            <span .misc>
              $forall (desc, value) <- xs
                <span><i>#{desc}:</i> #{value}
  <div .text-center .hl-on-hover style="margin-top:0.5em;">^{browseNavigation}
    |]
  where
    directory   = "directory"
    parameters  = [] -- TODO:    if' (perpage == 0) [] [("limit_to", T.pack $ show perpage)]
    playlistAddAll = [whamlet|
<a .btn
    onclick="playlist.add_from_element_contents($('.browser .type-file .data-field'), '#{lfSec config}'); return false"
    title="Adds all files in this folder.">Add all
    |]
    (browseSettings, browseNavigation) = (pagerRender <$> lfPaging
                                                      <*> lfNumPages
                                                      <*> (lfViewR <$> id <*> lfCurrentFPS) ) config
    brwidget = (simpleBreadcrumbs <$> lfSec
                                  <*> lfCurrentFPS
                                  <*> lfViewR) config

-- | Construct breadcrumbs into a widget.
simpleBreadcrumbs :: SectionId -> FPS -> (FPS -> Route master) -> WidgetT master IO ()
simpleBreadcrumbs _    []  _ = mempty
simpleBreadcrumbs home fps f = [whamlet|
<ul.breadcrumb>
  <li>
    <a .browser-link href=@{f mempty}><i>#{home}</i>
  <li .divider>&nbsp;&#x25B8;&nbsp;
  $forall (name, route) <- init parts
    <li>
      <a .browser-link href=@{route}>#{name}
    <li .divider>&nbsp;&#x25B8;&nbsp;
  <li.active>#{fst $ last parts}
|] where
  parts = map (second f) $ zip fps $ foldr (\x -> (:) [x] . map ([x] ++)) [[]] fps

-- | @pagerRender paging current@
pagerRender :: Paging -> Int -> Route master
            -> (WidgetT master IO (), WidgetT master IO ()) -- ^ (config, navigation)
pagerRender (perpage, curpage) pagecount r2c' =
    (,) [whamlet|$newline never
<span .btn-toolbar>
  $forall opt <- options
      $if perpage == opt
        <a .btn .btn-small .disabled>#{opt}
      $else
        <a .btn .btn-small .browser-link href="?limit_to=#{opt}&page=0">#{opt} #
|] $
    if' (length pages == 1) mempty [whamlet|$newline never
<span .btn-toolbar>
    $if curpage > 0
        <a .btn .btn-hl .btn-small .browser-link href="@?{r2c}&page=#{curpage - 1}">Previous
    $forall page <- pages
        $if (==) page (curpage + 1)
            <a .btn .btn-small .disabled>#{page}
        $else
            <a .btn .btn-small .browser-link href="@?{r2c}&page=#{page - 1}" > #{page}
    $if curpage < length pages
        <a .btn .btn-small .btn-hl .browser-link href="@?{r2c}&page=#{curpage + 1}">Next
    $else
        <a .btn .btn-small .disabled>Next
|] where
    r2c         = (r2c', [ ("limit_to", T.pack $ show perpage) ])
    options     = [20, 50, 100, 200]
    pages       = [1 .. pagecount]

-- | Get pager config.
pagerSettings :: HandlerT master IO Paging -- TODO: move somewhere (browser? sections helpers?)
pagerSettings = do
    limit <- liftM (maybe 50 (read . T.unpack)) $ lookupGetParam "limit_to"
    page  <- liftM (maybe 0  (read . T.unpack)) $ lookupGetParam "page"
    return (limit, page)
