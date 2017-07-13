#!/usr/bin/env stack
-- stack --resolver lts-8.22 --install-ghc runghc --package hakyll --package yaml
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Text (Text, unpack)
import System.FilePath (splitExtension, (<.>), (</>))
import Data.Monoid (mempty, (<>))
import Data.Aeson
import Data.Aeson.Types
import Data.Yaml
import Hakyll

main :: IO ()
main = hakyll $ do
  -- Compress CSS
  match "css/*" $ do
    route   idRoute
    compile compressCssCompiler

  -- Copy static files
  match "static/**" $ do
    route   idRoute
    compile copyFileCompiler

  match "publications.yaml" $ do
    route $ composeRoutes (setExtension ".html") appendIndex
    compile $ do
      pubpath <- getResourceFilePath
      res <- unsafeCompiler $ decodeFileEither pubpath
      case res of
        Left err -> error $ "Couldn't parse publications.yaml. Error: " <> show err
        Right pubs -> do
          let pubCtxt = field "title" (return . unpack . pTitle . itemBody)
                     <> field "url" (maybe (fail "") (return . unpack) . pUrl . itemBody)
                     <> field "authors" (return . unpack . pAuthors . itemBody)
                     <> field "venue" (return . unpack . pVenue . itemBody)
                     <> field "extra" (maybe (fail "") (return . unpack) . pExtra . itemBody)
          let pubsCtxt = listField "publications" pubCtxt (return $ map (Item "") pubs)
          let defCtxt = constField "is-publications" "true"
                     <> constField "title" "SILC: Publications"
                     <> defaultContext
          makeItem "" >>= loadAndApplyTemplate "templates/publications.html" pubsCtxt
                      >>= loadAndApplyTemplate "templates/base.html" defCtxt


  -- Compile markdown files
  match "*.markdown" $ do
    route $ composeRoutes (setExtension ".html") appendIndex

    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler

data Publication = Publication { pTitle :: Text
                               , pUrl :: Maybe Text
                               , pAuthors :: Text
                               , pVenue :: Text
                               , pExtra :: Maybe Text
                               }

instance FromJSON Publication where
  parseJSON (Object v) =
    Publication      <$>
    v .:  "title"    <*>
    v .:? "url"      <*>
    v .:  "authors"  <*>
    v .:  "venue"    <*>
    v .:? "extra"
  parseJSON invalid    = typeMismatch "Publication" invalid

appendIndex :: Routes
appendIndex = customRoute $
    (\(p, e) -> if p /= "index" then p </> "index" <.> e else p <.> e) . splitExtension . toFilePath
