{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module TH where

import qualified Data.ByteString.Lazy.Char8 as BL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.Environment
import System.Process.Typed

-- Fetch version information at compile-time using Template Haskell.
versions :: Q Exp
versions =
  let run cmd = runIO $ BL.unpack . fst <$> readProcess_ cmd
   in do
        ghcCommit <- run "ghc --print-project-git-commit-id"
        ghcVersion <- run "ghc --print-project-version"
        pandocVersion <- run "ghc-pkg field pandoc version --simple-output"

        -- passed in during `buildPhase`
        nixpkgsVersion <- runIO $ getEnv "RESUME_NIXPKGS_REV"

        [|
          [ ("ghc commit", ghcCommit),
            ("ghc version", ghcVersion),
            ("pandoc", pandocVersion),
            ("nixpkgs", nixpkgsVersion)
          ]
          |]
