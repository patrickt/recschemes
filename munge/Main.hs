{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Prelude            hiding (readFile, writeFile)

import           Control.Lens
import           Control.Monad
import           Debug.Trace
import           System.Environment
import           System.Exit
import           Text.HTML.DOM
import           Text.XML           (def, writeFile)
import           Text.XML.Lens

main :: IO ()
main = do
  args <- getArgs
  when (null args) (die "expected argument")
  contents <- readFile (head args)
  let modified = contents & root.entire.named "pre".attribute "data-language" ?~ "haskell"
  writeFile def (head args) modified
