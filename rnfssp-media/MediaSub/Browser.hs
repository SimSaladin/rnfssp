{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}
-- | Javascript browser, which utilises the onpopstate functionality of modern
-- browsers.
module MediaSub.Browser where

import           Prelude
import           Yesod
import           Data.Text (Text)
import           Data.Monoid
import           Control.Applicative
import           Control.Monad
import           Control.Arrow (second)
import qualified Data.Text as T
import           Text.Julius (rawJS)
import           Text.Coffee

import           Utils
 
-- * The browser framework

data BrowserSettings master = BrowserSettings
    { browserId         :: Text    -- ^ Identifier for the browser ("mybrowser")
    , browserLinksMatch :: Text    -- ^ (JQuery) matcher for browser links, e.g. "a.browser-link"
    , browserExtraDoms  :: [Text]  -- ^ Optional doms that can bind browser content. (DomId, )
    , browserContent    :: WidgetT master IO () -- ^ Initial/fallback content widget
    }

data ListViewConf = ListFlat Paging (Maybe Int) -- ^ paging, num of entries
    --  | ListBlocks
    --  | ListTree
    --  | ListBest

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

-- | The browser frame, this is where the all browser activities are
-- contained.
browserFrames :: BrowserSettings master -> WidgetT master IO ()
browserFrames config = [whamlet|$newline never
<div ##{browserId config}>^{browserContent config}
|]

-- | Just the (coffee-)script driving the browser.
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

-- * Paging

type Paging = (Int, Int) -- ^ (per page, current page)

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

-- | Construct breadcrumbs into a widget.
simpleBreadcrumbs :: Text -- ^ Top level
                  -> [Text] -> ([Text] -> Route master) -> WidgetT master IO ()
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
