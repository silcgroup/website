#!/usr/bin/env stack
-- stack --resolver lts-8.22 --install-ghc runghc --package hakyll --package yaml --package markdown
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Data.ByteString.Lazy as LBS
import Data.Text.Lazy (toStrict, fromStrict)
import qualified Data.Text as T
import Data.Text (Text, unpack)
import System.FilePath (splitExtension, (<.>), (</>))
import qualified Data.List as List
import Data.Monoid (mempty, (<>))
import Data.Traversable
import Data.Aeson.Types (typeMismatch)
import qualified Text.Markdown as MD
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Data.Yaml as Yaml
import Data.Yaml ((.:), (.:?), FromJSON)
import Hakyll

main :: IO ()
main = hakyll $ do
  -- Copy CNAME and favicon.ico
  match ("CNAME" .||. "favicon.ico") $ do
    route   idRoute
    compile copyFileCompiler

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
    let defCtxt = constField "is-publications" "true"
               <> constField "title" "SILC: Publications"
               <> defaultContext
    compile $ yamlCompiler
      >>= loadAndApplyTemplate "templates/publications.html" (listOfCtxt "publications" pubCtxt)
      >>= loadAndApplyTemplate "templates/base.html" defCtxt
      >>= relativizeUrls

  -- Build people page from YAML data
  match "people.yaml" $ do
    route $ composeRoutes (setExtension ".html") appendIndex
    let defCtxt = constField "is-people" "true"
               <> constField "title" "SILC: People"
               <> defaultContext
    compile $ yamlCompiler
      >>= loadAndApplyTemplate "templates/people.html" peopleCtxt
      >>= loadAndApplyTemplate "templates/base.html" defCtxt
      >>= relativizeUrls

  -- Build templates (used by the above)
  match "templates/*" $ compile templateBodyCompiler

personCtxt :: Context Person
personCtxt = tfield "name" rName
             <> tfield "role" rTitle
             <> tfield "homepage" rHomepage
             <> tfield "photo" rPhoto

groupCtxt :: Context (Text, [Person])
groupCtxt = tfield "group" fst
         <> (contramap snd $ listOfCtxt "people" personCtxt)

contramap :: (a -> b) -> Context b -> Context a
contramap f (Context c) = Context $ \s ss i -> c s ss (f <$> i)

peopleCtxt :: Context [Person]
peopleCtxt = listFieldWith "groups" groupCtxt (return . traverse separate)
  where separate :: [Person] -> [(Text, [Person])]
        separate ppl = let (currentPeople, formerPeople) = List.partition rCurrent ppl
                       in [("Current Members", currentPeople), ("Former Members", formerPeople)]


pubCtxt :: Context Publication
pubCtxt = tfield "title" pTitle
       <> mfield "url" pUrl
       <> tfield "authors" pAuthors
       <> tfield "venue" pVenue
       <> mfield "extra" pExtra

listOfCtxt :: String -> Context a -> Context [a]
listOfCtxt s ctx = listFieldWith s ctx (return . sequenceA)

-- NOTE(dbp 2017-07-14): The blaze html library makes it difficult (possibly
-- impossible?) to easily manipulate tags, so since we are really just trying to get rid of an extraneous wrapping <p>, we render and then do it in text.
stripP :: Text -> Text
stripP t = if "<p>" `T.isPrefixOf` t && "</p>" `T.isSuffixOf` t then T.dropEnd 4 $ T.drop 3 t else t

md :: Text -> Text
md s = stripP . toStrict . renderHtml . MD.markdown MD.def . fromStrict $ s

-- | Build field from string / getter function
tfield :: String -> (a -> Text) -> Context a
tfield s f = field s (return . unpack . md . f . itemBody)

-- | Build (potentially missing) optional field from string / getter function
mfield :: String -> (a -> Maybe Text) -> Context a
mfield s f = field s (maybe (fail "") (return . unpack . md) . f . itemBody)

yamlCompiler :: (FromJSON a) => Compiler (Item a)
yamlCompiler = do
  path <- getResourceFilePath
  rawItem <- getResourceLBS
  for rawItem $ \raw ->
    case Yaml.decodeEither . LBS.toStrict $ raw of
      Left err     -> error $ "Failed to parse " <> path <> " : " <> show err
      Right parsed -> return parsed

data Publication = Publication { pTitle :: Text
                               , pUrl :: Maybe Text
                               , pAuthors :: Text
                               , pVenue :: Text
                               , pExtra :: Maybe Text
                               }

instance FromJSON Publication where
  parseJSON (Yaml.Object v) =
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
                     , rCurrent :: Bool -- ^ True if current member, False if former member
                     }

instance FromJSON Person where
  parseJSON (Yaml.Object v) =
    Person      <$>
    v .: "name"     <*>
    v .: "title"    <*>
    v .: "homepage" <*>
    v .: "photo"    <*>
    (trueIfMissing <$> (v .:? "current"))
    where
      trueIfMissing :: Maybe Bool -> Bool
      trueIfMissing (Just b) = b
      trueIfMissing Nothing  = True
  parseJSON invalid    = typeMismatch "Person" invalid


appendIndex :: Routes
appendIndex = customRoute $
    (\(p, e) -> if p /= "index" then p </> "index" <.> e else p <.> e) . splitExtension . toFilePath
