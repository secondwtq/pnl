--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import           Hakyll.Web.Sass (sassCompiler)
import qualified Data.List as L


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "scss/*" $ do
        route   $ setExtension "css"
        let compiler = do
              css <- sassCompiler
              return $ compressCss <$> css
        compile compiler

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= saveSnapshot "teaser"
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/layout-html.html" postCtx
            >>= relativizeUrls

    match "index.html" $ do
        route idRoute
        compile $ do
            posts <- (take 3) <$> (recentFirst =<< loadAll "posts/*")

            let indexItemCtx =
                    teaserField "teaser" "teaser" <>
                    dateField "date" "%B %e, %Y" <>
                    defaultContext
            let applyLiTemplate post = loadAndApplyTemplate
                    "templates/post-list-index.html" indexItemCtx post
            let postsRendered = (mapM applyLiTemplate posts) >>= joinItems

            let indexCtx =
                    field "postsRendered" (const postsRendered) <>
                    constField "title" "Home" <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/layout-index.html" indexCtx
                >>= loadAndApplyTemplate "templates/layout-html.html" indexCtx
                >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/layout-other.html" archiveCtx
                >>= loadAndApplyTemplate "templates/layout-index.html" archiveCtx
                >>= loadAndApplyTemplate "templates/layout-html.html" archiveCtx
                >>= relativizeUrls

    match "about.rst" $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/layout-other.html" defaultContext
            >>= loadAndApplyTemplate "templates/layout-index.html" defaultContext
            >>= loadAndApplyTemplate "templates/layout-html.html" defaultContext
            >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" <>
    defaultContext

joinItems :: [Item String] -> Compiler String
joinItems = return . concat . (L.intersperse "\n") . (fmap itemBody)
