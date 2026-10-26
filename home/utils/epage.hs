{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import           Data.Functor       ((<$))

import           GHC.IO.Encoding    (setLocaleEncoding)

import           System.Directory   (removeFile)
import           System.Environment (getArgs, getEnv)
import           System.IO
import           System.IO.Error    (ioError, isDoesNotExistError, tryIOError)
import           System.Process     (runCommand)

import           Text.Printf        (printf)

main :: IO ()
main = do args <- getArgs
          let buffer   = arg "-b" "*STDIN*"        args
              filename = arg "-f" "emacsStdin.stdout" args
              mode     = arg "-m" "stdout-mode"    args

          tmpdir <- tryIOError (getEnv "TMPDIR") >>= \case
            Left err
              | isDoesNotExistError err -> pure "/tmp"
              | otherwise               -> ioError err
            Right dir -> pure dir
          let filepath = tmpdir <> "/" <> filename

          -- force UTF-8 to get around some weird behavior with Nix
          -- and locale settings (see
          -- https://github.com/NixOS/nixpkgs/issues/64603 for
          -- details)
          setLocaleEncoding utf8

          input <- getStdinUtf8
          if length (lines input) > 30
            then do
              writeFile filepath input
              () <$ runCommand (emacsBuffer filepath buffer mode)
            else do
              putStr input
  where -- helper functions to force UTF-8
        getStdinUtf8 = do
          hSetEncoding stdin utf8
          hGetContents stdin

-- | Template for the command we use to invoke emacsclient.
emacsclient :: String
emacsclient = "emacsclient --alternate-editor %s --eval '%s'"

emacsBuffer :: String -> String -> String -> String
emacsBuffer file buffer mode = printf emacsclient alternative elisp
  where alternative       = "'/usr/bin/env false'"
        letVals :: String = printf "(b (create-file-buffer \"%s\")) " buffer
        startFn :: String = printf "(switch-to-buffer b) (insert-file-contents \"%s\") (%s)" file mode
        elisp   :: String = printf "(let (%s) %s (delete-file \"%s\"))" letVals startFn file

arg :: String -> String -> [String] -> String
arg name def args = case take 2 $ dropWhile (/= name) args of
  [_, value] -> value
  _          -> def
