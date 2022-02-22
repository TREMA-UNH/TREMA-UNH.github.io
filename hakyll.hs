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

        match "content/**.mkd" $ do
                route   $ setExtension "html" `composeRoutes` gsubRoute "content/" (const "")
                compile $ pandocMathCompiler
                        >>= loadAndApplyTemplate "templates/default.html" defaultContext
                        >>= relativizeUrls

        match "templates/*" $ compile templateCompiler

pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler =
    let mathExtensions = extensionsFromList
            [ Ext_tex_math_dollars
            , Ext_tex_math_double_backslash
            , Ext_latex_macros
            ]
        defaultExtensions = writerExtensions defaultHakyllWriterOptions
        newExtensions = pandocExtensions <> mathExtensions <> extensionsFromList [Ext_citations]
        writerOptions = defaultHakyllWriterOptions {
                          writerExtensions = newExtensions,
                          writerHTMLMathMethod = MathJax ""
                        }
        readerOptions = defaultHakyllReaderOptions {
                          readerExtensions = newExtensions
                        }
        doCitations doc = unsafeCompiler $ do doc' <- handle (\(SomeException e) -> print e >> throw e) $ processCites' doc
                                              print doc'
                                              return doc'
    in pandocCompilerWithTransformM readerOptions writerOptions doCitations

