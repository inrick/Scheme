module Main where

import Control.Monad (liftM)
import System.Environment

import Scheme.Error
import Scheme.Eval
import Scheme.Parser

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
