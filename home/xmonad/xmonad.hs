{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import System.Exit

import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.ManageHelpers

import XMonad.Layout.Gaps (gaps)
import XMonad.Layout.NoBorders (noBorders)

import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Prompt.Shell

import XMonad.Util.EZConfig
import XMonad.Util.Run (safeSpawn)
import XMonad.Util.Types (Direction2D (..))

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
      ||| noBorders Full
      ||| noBorders (gaps [(D, 720), (R, 1280)] Full)
