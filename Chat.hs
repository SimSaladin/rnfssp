{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FlexibleContexts, ScopedTypeVariables
  #-}
------------------------------------------------------------------------------
-- File:          Chat.hs
-- Creation Date: Jul 15 2012 [15:27:50]
-- Last Modified: Sep 14 2013 [23:12:19]
-- Created By:    Samuli Thomasson [SimSaladin] samuli.thomassonAtpaivola.fi
--
-- Credits:       http://www.yesodweb.com/book/wiki-chat-example
------------------------------------------------------------------------------

-- | This modules defines a subsite that allows you to insert a chat box on
-- any page of your site. It uses eventsource for sending the messages from
-- the server to the browser.
module Chat 
    ( Chat(..)
    , Route(..)
    , resourcesChat
    , YesodChat(..)
    , chatWidget
    , postSendR, getReceiveR
    ) where

import Prelude
import Yesod
import Blaze.ByteString.Builder (Builder, toByteString)
import Control.Concurrent.Chan (Chan, dupChan, writeChan)
import Data.Foldable
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Wai (Response(ResponseSource), responseSource)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan)
import Text.Julius (rawJS)

data Chat = Chat (Chan ServerEvent)
mkYesodSubData "Chat" [parseRoutes|
/send SendR POST
/recv ReceiveR GET
|]

-- | Chat interface
class (Yesod master, RenderMessage master FormMessage) => YesodChat master where

    -- | The message datatype
    data ChatMessage master

    chatRenderMsg :: ChatMessage master -> Builder

    chatCreateMsg :: Text -> Text -> HandlerT master IO (ChatMessage master)

    -- | Retrieve the chat ident, nick, for the user. Nothing if no nick is
    -- available (=> chat is disabled)
    chatIdent :: HandlerT master IO (Maybe Text)

    chatGet   :: HandlerT master IO [ChatMessage master]


-- *

type Handler master = HandlerT Chat (HandlerT master IO)

-- | Get a message from the user and send it to all listeners.
postSendR :: YesodChat master => Handler master ()
postSendR = lift chatIdent >>= traverse_ f
    where f ident = do
                Chat chan <- getYesod
                body <- lift (runInputGet $ ireq textField "message")
                message <- lift $ chatCreateMsg ident body

                -- Set nginx-specific header for eventsource to work.
                addHeader "X-Accel-Buffering" "no"

                -- Send an event to all listeners with the user's name and message.
                liftIO $ writeChan chan $ ServerEvent Nothing Nothing
                                          [chatRenderMsg message]

-- | Send an eventstream response with all messages streamed in.
getReceiveR :: Handler master ()
getReceiveR = do
    Chat chan0 <- getYesod
    chan <- liftIO $ dupChan chan0
    req <- waiRequest
    res <- liftResourceT $ eventSourceAppChan chan req
    let (stat, hs, src) = responseSource res

    -- for nginx reverse-proxying to work, we add the header X-Accel-Buffering
    sendWaiResponse $ ResponseSource stat (("X-Accel-Buffering", "no"):hs) src

-- | Provide a widget that the master site can embed on any page.
chatWidget :: (YesodChat master, ChatMessage master ~ ChatMessage master0)
           => (Route Chat -> Route master) -> WidgetT master IO ()
chatWidget toMaster = do
    let disabledChat = do
            master <- liftHandlerT getYesod
            [whamlet|
<h1 .icon-chat> Chat
<p>
    You must be #
    $maybe ar <- authRoute master
        <a href=@{ar}>logged in
    $nothing
        logged in
    \ to chat.
|]
    mident <- liftHandlerT chatIdent -- check if we're already logged in
    flip (maybe disabledChat) mident $ \_ -> do

        recent  <- liftHandlerT chatGet
        chat    <- liftHandlerT newIdent   -- the containing div

        [whamlet|
<h1 .icon-chat> Chat
<div.clearfix ##{chat}>
    <div ##{chat}-output>
      $forall msg <- recent
        #{preEscapedToMarkup $ decodeUtf8 $ toByteString $ chatRenderMsg msg}
    <input ##{chat}-input type=text placeholder="Enter Message">
|] >> toWidget [lucius|
 ##{chat}-output { width: 100%; max-height: 300px; overflow: auto; }
 ##{chat}-input { width: 100%; padding-left:0; padding-right:0; }
|] >> toWidgetBody [julius|
// Set up the receiving end
var output = document.getElementById("#{rawJS chat}-output");
var src = new EventSource("@{toMaster ReceiveR}");
src.onmessage = function(msg) {
    // This function will be called for each new message.
    output.innerHTML = output.innerHTML + msg.data;

    // And now scroll down within the output div so the most recent message
    // is displayed.
    output.scrollTop = output.scrollHeight;
};

// Set up the sending end: send a message via Ajax whenever the user hits
// enter.
var input = document.getElementById("#{rawJS chat}-input");
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
