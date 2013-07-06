{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts
  #-}
------------------------------------------------------------------------------
-- File:          Chat.hs
-- Creation Date: Jul 15 2012 [15:27:50]
-- Last Modified: Jul 06 2013 [14:02:32]
-- Created By:    Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
--
-- Credits:       http://www.yesodweb.com/book/wiki-chat-example
------------------------------------------------------------------------------

-- | This modules defines a subsite that allows you to insert a chat box on
-- any page of your site. It uses eventsource for sending the messages from
-- the server to the browser.
module Chat where

import Yesod
import Prelude
import Text.Julius (rawJS)
import Prelude (Bool, ($), Maybe(..), (.))
import Control.Concurrent.Chan (Chan, dupChan, writeChan)
import Data.Text (Text)
import Network.Wai (Response(ResponseSource), responseSource)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Blaze.ByteString.Builder.Char.Utf8 (fromText)
import Data.Monoid (mappend)

data Chat = Chat (Chan ServerEvent)

class (Yesod master, RenderMessage master FormMessage) => YesodChat master where
    getUserName :: HandlerT master IO Text
    isLoggedIn  :: HandlerT master IO Bool
    saveMessage :: (Text, Text) -> HandlerT master IO ()
    getRecent   :: HandlerT master IO [(Text, Text)]


mkYesodSubData "Chat"
    --[ ClassP ''YesodChat [VarT $ mkName "master"] ]
    [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]

-- | Get a message from the user and send it to all listeners.
postSendR :: YesodChat master => HandlerT Chat (HandlerT master IO) ()
postSendR = do
    from <- lift getUserName
    body <- lift $ runInputGet $ ireq textField "message"
    Chat chan <- getYesod

    -- Set nginx-specific header for eventsource to work.
    addHeader "X-Accel-Buffering" "no"

    -- Send an event to all listeners with the user's name and message.
    liftIO $ writeChan chan $ ServerEvent Nothing Nothing $ return $
        fromText from `mappend` fromText ": " `mappend` fromText body

    lift $ saveMessage (from, body)

-- | Send an eventstream response with all messages streamed in.
getReceiveR :: HandlerT Chat (HandlerT master IO) ()
getReceiveR = do
    Chat chan0 <- getYesod
    chan <- liftIO $ dupChan chan0
    req <- waiRequest
    res <- liftResourceT $ eventSourceAppChan chan req
    let (stat, hs, src) = responseSource res

    -- for nginx reverse-proxying to work, we add the header X-Accel-Buffering
    sendWaiResponse $ ResponseSource stat (("X-Accel-Buffering", "no"):hs) src

-- | Provide a widget that the master site can embed on any page.
chatWidget :: (YesodChat master)
           => (Route Chat -> Route master)
           -> WidgetT master IO ()
chatWidget toMaster = do
    chat    <- liftHandlerT newIdent   -- the containing div
    output  <- liftHandlerT newIdent   -- the box containing the messages
    input   <- liftHandlerT newIdent   -- input field from the user
    recent  <- liftHandlerT getRecent
    ili     <- liftHandlerT isLoggedIn -- check if we're already logged in
    if ili
        then do
            -- Logged in: show the widget
            [whamlet|
<h6 .icon-chat> Chat
<div.clearfix ##{chat}>
    <div ##{output} .chat-output>
      $forall (poster, msg) <- recent
        <p>
            <span .poster>#{poster}: #
            #{msg}
    <input ##{input} type=text placeholder="Enter Message">
|]
            toWidget [lucius|
 ##{output} {
    width: 100%;
    max-height: 300px;
    overflow: auto;
} ##{input} {
   width: 100%;
   padding-left:0;
   padding-right:0;
}
|]
            -- And now that Javascript
            toWidgetBody [julius|
// Set up the receiving end
var output = document.getElementById("#{rawJS output}");
var src = new EventSource("@{toMaster ReceiveR}");
src.onmessage = function(msg) {
    // This function will be called for each new message.
    var p = document.createElement("p");
    p.appendChild(document.createTextNode(msg.data));
    output.appendChild(p);

    // And now scroll down within the output div so the most recent message
    // is displayed.
    output.scrollTop = output.scrollHeight;
};

// Set up the sending end: send a message via Ajax whenever the user hits
// enter.
var input = document.getElementById("#{rawJS input}");
input.onkeyup = function(event) {
    var keycode = (event.keyCode ? event.keyCode : event.which);
    if (keycode == '13') {
        var xhr = new XMLHttpRequest();
        var val = input.value;
        input.value = "";
        var params = "?message=" + encodeURIComponent(val);
        xhr.open("POST", "@{toMaster SendR}" + params);
        xhr.send(null);
    }
}
|]
        else do
            -- User isn't logged in, give a not-logged-in message.
            master <- liftHandlerT getYesod
            [whamlet|
<h6 .icon-chat> Chat
<p>
    You must be #
    $maybe ar <- authRoute master
        <a href=@{ar}>logged in
    $nothing
        logged in
    \ to chat.
|]
