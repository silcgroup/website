#!/usr/bin/env stack
-- stack --resolver lts-8.22 --install-ghc runghc --package hakyll
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.FilePath (splitExtension, (<.>), (</>))
import Data.Monoid (mempty)
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

  -- Compile markdown files
  match "*.markdown" $ do
    route $ composeRoutes (setExtension ".html") appendIndex
    
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/base.html" defaultContext
      >>= relativizeUrls

  match "templates/*" $ compile templateBodyCompiler


appendIndex :: Routes
appendIndex = customRoute $
    (\(p, e) -> if p /= "index" then p </> "index" <.> e else p <.> e) . splitExtension . toFilePath
