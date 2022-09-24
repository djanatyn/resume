{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text as T (Text, concat, take, unpack)
import qualified Data.Text.IO as TIO
import Dhall (FromDhall, Generic, auto, inputFile)
import System.Process.Typed (readProcess_)
import TH (Versions (..), versions)
import qualified Text.Blaze as B
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import Text.Blaze.Html5 as BH
import qualified Text.Blaze.Html5.Attributes as A

data Job where
  Job ::
    { organization :: Text,
      position :: Text,
      duration :: Text,
      experiences :: [Text]
    } ->
    Job
  deriving (Show, Generic, FromDhall)

data ContactInfo where
  ContactInfo ::
    { name :: Text,
      email :: Text,
      github :: Text
    } ->
    ContactInfo
  deriving (Show, Generic, FromDhall)

data Resume where
  Resume ::
    { contact :: ContactInfo,
      history :: [Job]
    } ->
    Resume
  deriving (Show, Generic, FromDhall)

data Icons where
  Icons ::
    { haskell :: BH.Html,
      nixos :: BH.Html,
      rust :: BH.Html,
      elm :: BH.Html
    } ->
    Icons

loadIcon :: FilePath -> IO BH.Html
loadIcon path = BH.preEscapedToHtml <$> TIO.readFile path

loadIcons :: IO Icons
loadIcons = do
  haskell <- loadIcon "icons/haskell.svg"
  nixos <- loadIcon "icons/nixos.svg"
  rust <- loadIcon "icons/rust.svg"
  elm <- loadIcon "icons/elm.svg"

  pure $ Icons {haskell, nixos, rust, elm}

loadResume :: IO Resume
loadResume = inputFile (auto @Resume) "./experience.dhall"

renderJob :: Job -> BH.Html
renderJob Job {organization, position, duration, experiences} = do
  BH.div ! A.class_ "box" $ do
    BH.div ! A.class_ "organization" $ BH.p $ BH.toHtml organization
    BH.div ! A.class_ "position" $ BH.p $ BH.toHtml position <> BH.toHtml ("(" ++ unpack duration ++ ")")
  BH.ul $ mapM_ (BH.li . BH.toHtml) experiences

main :: IO ()
main = do
  -- load resume
  Resume {contact = ContactInfo {name, email, github}, history} <- loadResume
  Icons {haskell, nixos, rust, elm} <- loadIcons

  -- render html
  writeFile "resume.html" $
    renderHtml $ do
      BH.h1 $ BH.toHtml name
      BH.hr
      BH.div ! A.class_ "box" $ do
        BH.div $ BH.a ! A.href (B.textValue github) $ BH.h4 $ BH.toHtml github
        BH.div $ haskell >> nixos >> rust >> elm
        BH.div $ BH.a ! A.href (B.textValue $ T.concat ["mailto:", email]) $ BH.h4 $ BH.toHtml email
      BH.h3 $ BH.string "Work Experience"
      mapM_ renderJob history
      BH.hr
      BH.h3 ! A.class_ "right" $ do
        "(this resume "
        BH.a ! A.href "https://github.com/djanatyn/resume/blob/master/flake.nix" $ "built with..."
        ")"
      let Versions {nixpkgsRev, ghcVersion, ghcRev} = $(versions)
          nixpkgsUrl = T.concat ["https://github.com/NixOS/nixpkgs/tree/", nixpkgsRev]
          ghcUrl = T.concat ["https://gitlab.haskell.org/ghc/ghc/-/tree/", ghcRev]
          nixpkgsText = T.concat ["NixOS/nixpkgs (", T.take 8 nixpkgsRev, ")"]
          ghcText = T.concat ["GHC ", ghcVersion, " (", T.take 8 ghcRev, ")"]
       in BH.ul $ do
            BH.li $ BH.a ! A.href (B.textValue nixpkgsUrl) $ B.text nixpkgsText
            BH.li $ BH.a ! A.href (B.textValue ghcUrl) $ B.text ghcText

  -- convert html to pdf
  (stdout, stderr) <- readProcess_ "wkhtmltopdf --user-style-sheet style.css resume.html resume.pdf"
  putStrLn (BL.unpack stderr)
