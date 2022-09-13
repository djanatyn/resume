{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad (mapM_)
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text, unpack)
import Dhall (FromDhall, Generic, auto, inputFile)
import System.IO (writeFile)
import System.Process.Typed (readProcess_)
import qualified TH
import Text.Blaze.Html.Renderer.Pretty (renderHtml)
import qualified Text.Blaze.Html5 as BH
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

data Skills where
  Skills ::
    { languages :: [Text],
      software :: [Text]
    } ->
    Skills
  deriving (Show, Generic, FromDhall)

data ContactInfo where
  ContactInfo ::
    { name :: Text,
      email :: Text
    } ->
    ContactInfo
  deriving (Show, Generic, FromDhall)

data Resume where
  Resume ::
    { contact :: ContactInfo,
      skills :: Skills,
      history :: [Job]
    } ->
    Resume
  deriving (Show, Generic, FromDhall)

loadResume :: IO Resume
loadResume = inputFile (auto @Resume) "./experience.dhall"

renderJob :: Job -> BH.Html
renderJob Job {organization, position, duration, experiences} = do
  BH.h2 $ BH.toHtml organization
  BH.h4 $ BH.toHtml position <> BH.toHtml ("(" ++ unpack duration ++ ")")
  BH.ul $ mapM_ (BH.li . BH.toHtml) experiences

versions :: [String]
versions = $(TH.versions)

main :: IO ()
main = do
  -- load resume
  Resume {contact, skills, history} <- loadResume

  -- render html
  writeFile "resume.html" $
    renderHtml $
      let text = BH.toHtml @String
       in do
            BH.h1 $ BH.toHtml $ name contact
            BH.hr
            BH.h4 $ BH.toHtml $ email contact
            BH.h2 $ text "Skills"
            BH.ul $ mapM_ (BH.li . BH.toHtml) $ languages skills
            BH.hr
            BH.ul $ mapM_ (BH.li . BH.toHtml) $ software skills
            BH.h1 $ text "Work Experience"
            mapM_ renderJob history
            BH.hr
            BH.h3 $ text "(this resume built with...)"
            BH.ul $ mapM_ (BH.li . BH.toHtml) versions

  -- convert html to pdf
  (stdout, stderr) <- readProcess_ "wkhtmltopdf --user-style-sheet style.css resume.html resume.pdf"
  putStrLn (BL.unpack stderr)
