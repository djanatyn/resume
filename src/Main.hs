{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text.IO as TI
import System.IO (writeFile)
import System.Process.Typed (readProcess_)
import qualified TH
import Text.Pandoc (def, runIOorExplode, writeHtml5String)
import Text.Pandoc.Builder

versions :: [(String, String)]
versions = $(TH.versions)

myDoc :: Pandoc
myDoc =
  setTitle "Resume" $
    doc $
      para "paragraph 1"
        <> para "paragraph 2"
        <> bulletList
          [ para "item 1",
            para "item 2" <> para "item 2 line 2",
            plain (link "/url" "go to url" "link")
          ]

main :: IO ()
main = do
  html <- runIOorExplode $ writeHtml5String def myDoc
  TI.writeFile "resume.html" html

  (stdout, stderr) <- readProcess_ "wkhtmltopdf resume.html resume.pdf"

  putStrLn (BL.unpack stderr)
