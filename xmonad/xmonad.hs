module Main where

import System.Exit

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageHelpers

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn)

main :: IO ()
main = xmonad $ baseConfig `additionalKeysP` keybindings

baseConfig = desktopConfig
  { modMask = mod4Mask
  , borderWidth = 2
  , terminal = "emacsclient -c"
  }

keybindings =
  [ ("M-q", safeSpawn "home-manager" ["switch"])
  , ("M-S-q", confirmPrompt promptConfig "exit" (io exitSuccess))
  ]

promptConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  }
