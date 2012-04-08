module Handler.Board where

import Import

getBoardR :: Handler RepHtml
getBoardR = do
    (formWidget, formEnctype) <- generateFormPost boardForm
    let submission = Nothing :: Maybe (Text, Text, Text)
        handlerName = "getBoardR" :: Text
    defaultLayout $ do
        setTitle "Welcome SS/board!"
        $(widgetFile "board")

postBoardR :: Handler RepHtml
postBoardR = do
    ((result, formWidget), formEnctype) <- runFormPost boardForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        setTitle "Thanks for posting!"
        $(widgetFile "board")

boardForm :: Form Boardpost
boardForm = renderDivs $ Boardpost
    <$> areq textField "Title" Nothing
    <*> areq textField "Name" Nothing
    <*> areq textareaField "Content" Nothing
    <*> areq hiddenField "parent" Nothing

