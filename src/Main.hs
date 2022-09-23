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
import Data.Text as T (Text, concat, unpack)
import Dhall (FromDhall, Generic, auto, inputFile)
import System.IO (writeFile)
import System.Process.Typed (readProcess_)
import qualified TH
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

loadResume :: IO Resume
loadResume = inputFile (auto @Resume) "./experience.dhall"

renderJob :: Job -> BH.Html
renderJob Job {organization, position, duration, experiences} = do
  BH.div ! A.class_ "box" $ do
    BH.div ! A.class_ "organization" $ BH.p $ BH.toHtml organization
    BH.div ! A.class_ "position" $ BH.p $ BH.toHtml position <> BH.toHtml ("(" ++ unpack duration ++ ")")
  BH.ul $ mapM_ (BH.li . BH.toHtml) experiences

versions :: [String]
versions = $(TH.versions)

main :: IO ()
main = do
  -- load resume
  Resume {contact, history} <- loadResume

  -- render html
  writeFile "resume.html" $
    renderHtml $
      let text = BH.toHtml @String
       in do
            BH.h1 $ BH.toHtml $ name contact
            BH.hr
            BH.div ! A.class_ "box" $
              let ContactInfo {name, email, github} = contact
               in do
                    BH.div $ BH.a ! A.href (B.textValue github) $ BH.h4 $ BH.toHtml $ github
                    BH.div $ BH.a ! A.href (B.textValue $ T.concat ["mailto:", email]) $ BH.h4 $ BH.toHtml $ email
            BH.h3 $ text "Work Experience"
            mapM_ renderJob history
            BH.hr
            BH.h3 ! A.class_ "right" $ text "(this resume built with...)"
            BH.ul $ mapM_ (BH.li . BH.toHtml) versions

  -- convert html to pdf
  (stdout, stderr) <- readProcess_ "wkhtmltopdf --user-style-sheet style.css resume.html resume.pdf"
  putStrLn (BL.unpack stderr)
