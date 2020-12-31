#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (ps: [ps.turtle])"

{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE UnicodeSyntax       #-}

import qualified Data.Map as Map
import Data.Map (Map)

import Prelude hiding (FilePath)

import Turtle

-- * home-manager operations

main = sh $ join $
  options "Build or install home-manager environment." $
       subcommand "build"  "Build config without installing." (build <$> nixOptions)
   <|> subcommand "switch" "Build and install config."        (switch' <$> nixOptions)

switch' passthrough = do
  work ← ensureDir "WORK_DIR"
  build passthrough { outLink = Just $ work </> "generation" }
  shell (toText' $ work </> "generation" </> "activate") empty

build passthrough = do
  homeConfig ← need "HOME_MANAGER_CONFIG" >>= \case
    Just value → pure value
    Nothing    → die "HOME_MANAGER_CONFIG unset, stopping."

  let nixOptions = passthrough
        { args = args passthrough <> [("confPath", homeConfig)]
        , attr = Just "activationPackage"
        }

  nixBuild "<home-manager/home-manager/home-manager.nix>" nixOptions

-- * nix-build

data NixOptions = NixOptions
  { includes ∷ [FilePath]
  , verbose  ∷ Bool
  , args     ∷ Map Text Text
  , attr     ∷ Maybe Text
  , outLink  ∷ Maybe FilePath
  }

nixOptions = NixOptions
  <$> many (optPath "include" 'I' "Add a path to the Nix search path.")
  <*> switch "verbose" 'v' "Verbose output."
  <*> pure Map.empty
  <*> optional (optText "attr" 'A' "Attribute to build from home.nix file.")
  <*> pure Nothing

nixBuild target NixOptions {..} = do
  readonly ← not . _writable <$> getmod "."
  when readonly $ die "Cannot run in read-only directory."

  proc "nix-build" (target : options) empty
  where options =
          ["--show-trace" | verbose ] <> nixIncludes <> nixArgs <> nixOut

        nixIncludes = do
          file ← includes
          ["-I", toText' file]
        nixArgs = do
          (name, value) ← Map.toList args
          ["--argstr", name, value]
        nixOut = case outLink of
          Just out → ["--out-link", toText' out]
          Nothing  → []

-- * Shell Helpers

ensureDir var = do
  dir    ← maybe "" fromText <$> need var
  exists ← testdir dir
  if exists then pure dir else mktempdir "." "home-manager-build"

toText' = either id id . toText
