{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import System.Exit

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.MultiColumns

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn)

main :: IO ()
main = xmonad $ baseConfig `additionalKeysP` keybindings

baseConfig = desktopConfig
  { modMask     = mod4Mask
  , borderWidth = 2
  , terminal    = "emacsclient -c"
  , layoutHook  = layouts
  }

keybindings =
  [ ("M-q",   safeSpawn "home-manager" ["switch"])
  , ("M-S-q", confirmPrompt promptConfig "exit" (io exitSuccess))
  ]

promptConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  }

-- * Layouts

layouts = Tall 1 (3 / 100) (1 / 2)
      ||| Full
      ||| multiCol [1] 1 0.01 (-0.5)
