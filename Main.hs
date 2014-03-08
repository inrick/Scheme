module Main where

import System.Environment (getArgs)

import Scheme.IO

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> evalAndPrint $ args !! 0
            _ -> putStrLn "Only 0 or 1 arguments allowed"
