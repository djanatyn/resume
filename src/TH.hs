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
        -- passed in during `buildPhase`
        nixpkgsVersion <- runIO $ getEnv "RESUME_NIXPKGS_REV"

        ghcCommit <- run "ghc --print-project-git-commit-id"
        ghcVersion <- run "ghc --print-project-version"

        [|
          map
            unwords
            [ ["ghc", ghcVersion, "- rev", ghcCommit],
              ["nixos/nixpkgs - rev", nixpkgsVersion]
            ]
          |]
