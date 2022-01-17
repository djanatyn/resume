{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified TH
import Data.Text as T
import Text.Pandoc
import Text.Pandoc.Builder
import Text.Pandoc.Writers.HTML

myDoc :: Pandoc
myDoc =
  setTitle "title" $
    doc $
      para "content"

versions :: [(String, String)]
versions = $(TH.versions)

main :: IO ()
main = do
  output <- runIOorExplode $ writeHtml5String def myDoc
  print versions
