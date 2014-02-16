module Main where

import Control.Monad (liftM)
import System.Environment (getArgs)

import Scheme.Error
import Scheme.Eval (eval)
import Scheme.Parser (readExpr)

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled
