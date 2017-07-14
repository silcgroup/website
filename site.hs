#!/usr/bin/env stack
-- stack --resolver lts-8.22 --install-ghc runghc --package hakyll --package yaml
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
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

  -- Compile all markdown files
  match "*.markdown" $ do
    route $ composeRoutes (setExtension ".html") appendIndex

    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= relativizeUrls

  -- Build publications page from YAML data
  match "publications.yaml" $ do
    route $ composeRoutes (setExtension ".html") appendIndex
    compileYaml $ \pubs -> do
      let pubCtxt = tfield "title" pTitle
                 <> mfield "url" pUrl
                 <> tfield "authors" pAuthors
                 <> tfield "venue" pVenue
                 <> mfield "extra" pExtra
      let pubsCtxt = listField "publications" pubCtxt (return pubs)
      let defCtxt = constField "is-publications" "true"
                 <> constField "title" "SILC: Publications"
                 <> defaultContext
      makeItem "" >>= loadAndApplyTemplate "templates/publications.html" pubsCtxt
                  >>= loadAndApplyTemplate "templates/base.html" defCtxt
                  >>= relativizeUrls

  -- Build people page from YAML data
  match "people.yaml" $ do
    route $ composeRoutes (setExtension ".html") appendIndex
    compileYaml $ \people -> do
      let personCtxt = tfield "name" rName
                    <> tfield "role" rTitle
                    <> tfield "homepage" rHomepage
                    <> tfield "photo" rPhoto
      let peopleCtxt = listField "people" personCtxt (return people)
      let defCtxt = constField "is-people" "true"
                 <> constField "title" "SILC: People"
                 <> defaultContext
      makeItem "" >>= loadAndApplyTemplate "templates/people.html" peopleCtxt
                  >>= loadAndApplyTemplate "templates/base.html" defCtxt
                  >>= relativizeUrls

  -- Build templates (used by the above)
  match "templates/*" $ compile templateBodyCompiler


-- | Build field from string / getter function
tfield s f = field s (return . unpack . f . itemBody)
-- | Build (potentially missing) optional field from string / getter function
mfield s f = field s (maybe (fail "") (return . unpack) . f . itemBody)

compileYaml f = compile $ do
      pubpath <- getResourceFilePath
      res <- unsafeCompiler $ decodeFileEither pubpath
      case res of
        Left err -> error $ "Couldn't parse " <> pubpath <> ". Error: " <> show err
        Right parsed -> f (map (Item "") parsed)

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


data Person = Person { rName :: Text
                     , rTitle :: Text
                     , rHomepage :: Text
                     , rPhoto :: Text
                     }

instance FromJSON Person where
  parseJSON (Object v) =
    Person      <$>
    v .: "name"     <*>
    v .: "title"    <*>
    v .: "homepage" <*>
    v .: "photo"
  parseJSON invalid    = typeMismatch "Person" invalid


appendIndex :: Routes
appendIndex = customRoute $
    (\(p, e) -> if p /= "index" then p </> "index" <.> e else p <.> e) . splitExtension . toFilePath
