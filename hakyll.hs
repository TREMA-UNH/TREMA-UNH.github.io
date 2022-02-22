{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (id)
import qualified Data.Set as S

import Hakyll
import Data.Monoid (mempty, mconcat, (<>))
import Control.Exception
import Text.Pandoc
import Text.CSL.Pandoc

main :: IO ()
main = hakyll $ do
        match "css/*" $ do
                route   idRoute
                compile compressCssCompiler

        match "content/**.png" $ do
                route   $ gsubRoute "content/" (const "")
                compile copyFileCompiler

        match "content/**.mkd" $ do
                route   $ setExtension "html" `composeRoutes` gsubRoute "content/" (const "")
                compile $ pandocCompiler
                        >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= relativizeUrls

        match "templates/*" $ compile templateCompiler

