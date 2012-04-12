{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Text (append)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler RepHtml
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost sampleForm
    let submission = Nothing :: Maybe (FileInfo, Text)
        handlerName = "getHomeR" :: Text
    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

postHomeR :: Handler RepHtml
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- lift newIdent
        setTitle "Welcome To Yesod!"
        $(widgetFile "homepage")

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing

getAdminR :: Handler RepHtml
getAdminR = do
    (boardAddWidget, encType) <- generateFormPost newboardForm
    let submission = Nothing :: Maybe Board
    defaultLayout $ do
        setTitle "admin"
        $(widgetFile "admin")

postAdminR :: Handler RepHtml
postAdminR = do
    ((result, boardAddWidget), encType) <- runFormPost newboardForm
    boards <- runDB $ selectList ([] :: [Filter Board]) []
    case result of
        FormSuccess board -> do
            _ <- runDB $ insert board
            setMessage $ toHtml $ "Uusi lauta luotu: " `append` (boardName board)
        _ -> do
            setMessage "Hmmm.. jokin meni pieleen lautaa luotaessa"
    defaultLayout $ do
        setTitle "Lauta"
        $(widgetFile "admin")

newboardForm :: Form Board
newboardForm = renderDivs $ Board
    <$> areq textField "Nimi (== url)" Nothing
    <*> areq textField "Kuvaus" Nothing
