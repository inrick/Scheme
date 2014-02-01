module Main where

import System.Environment

import Scheme.Eval
import Scheme.Parser

main :: IO ()
main = print . eval . readExpr . (!! 0) =<< getArgs
