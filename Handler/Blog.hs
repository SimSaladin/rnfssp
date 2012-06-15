module Handler.Blog
    ( getBlogOverviewR
    , postBlogOverviewR
    , getBlogViewR
    , postBlogViewR
    , getBlogEditR
    , postBlogEditR
    ) where

import Import

getBlogOverviewR :: Handler RepHtml
getBlogOverviewR = do
    defaultLayout $ do
        setTitle "Blog"
        $(widgetFile "blog-home")

postBlogOverviewR :: Handler RepHtml
postBlogOverviewR = redirect $ BlogOverviewR

getBlogEditR :: BlogpostId -> Text -> Handler RepHtml
getBlogEditR id title = do
    defaultLayout $ do
        setTitle "Editing"
        $(widgetFile "blog-home")

postBlogEditR :: BlogpostId -> Text -> Handler RepHtml
postBlogEditR id title = redirect $ BlogEditR id title

getBlogViewR :: BlogpostId -> Text -> Handler RepHtml
getBlogViewR id title = do
    defaultLayout $ do
        setTitle "wut?"
        $(widgetFile "blog-home")

postBlogViewR :: BlogpostId -> Text -> Handler RepHtml
postBlogViewR id title = redirect $ BlogViewR id title

