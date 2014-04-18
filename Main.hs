module Main where

import System.Environment (getArgs)

import Scheme.IO

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> runRepl
            _  -> runOne args
