{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text as T (Text, pack, strip)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment
import System.Process.Typed

data Versions where
  Versions ::
    { nixpkgsRev :: Text,
      ghcVersion :: Text,
      ghcRev :: Text
    } ->
    Versions
  deriving (Show)

run :: ProcessConfig stdin stdoutIgnored stderrIgnored -> Q Text
run cmd = runIO $ T.strip . T.pack . BL.unpack . fst <$> readProcess_ cmd

-- Fetch version information at compile-time using Template Haskell.
versions :: Q Exp
versions = do
  -- passed in during `buildPhase`
  nixpkgsRev <- runIO $ pack <$> getEnv "RESUME_NIXPKGS_REV"
  ghcRev <- run "ghc --print-project-git-commit-id"
  ghcVersion <- run "ghc --print-project-version"

  [|Versions {nixpkgsRev, ghcRev, ghcVersion}|]
