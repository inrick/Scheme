module Scheme.IO where

import Control.Monad (liftM)
import Data.IORef
import System.IO

import Scheme.Data
import Scheme.Eval
import Scheme.Parser

flushStr :: String -> IO ()
flushStr s = putStr s >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: IORef Env -> String -> IO String
evalString env expr = runIOThrows $
                        liftM show ((liftThrows $ readExpr expr) >>= eval env)

evalAndPrint :: IORef Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
  result <- prompt
  if predicate result then
    return ()
  else
    action result >> until_ predicate prompt action

runOne :: [String] -> IO ()
runOne args = do
  env <- primBinds >>=
    flip bindVars [("args", List . map String . drop 1 $ args)]
  (runIOThrows . liftM show . eval env $
    List [Atom "load", String $ args !! 0])
    >>= hPutStrLn stderr

runRepl :: IO ()
runRepl = primBinds >>= until_ (== "quit") (readPrompt "λ> ") . evalAndPrint
