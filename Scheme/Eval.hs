{-# LANGUAGE ExistentialQuantification #-}
module Scheme.Eval where

import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Monad.Error
import Data.IORef

import Scheme.Error
import Scheme.Data

eval :: IORef Env -> LispVal -> IOThrowsError LispVal
eval _ val@(Bool   _) = return val
eval _ val@(Number _) = return val
eval _ val@(Float  _) = return val
eval _ val@(Char   _) = return val
eval _ val@(String _) = return val
eval env (Atom atom) = getVar env atom
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "set!", Atom var, form]) =
  eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
  eval env form >>= defineVar env var
eval env (List [Atom "if", cond, thenBranch, elseBranch]) = do
  result <- eval env cond
  case result of
    Bool True  -> eval env thenBranch
    Bool False -> eval env elseBranch
    x          -> throwError $ TypeMismatch "bool" x
eval env (List (Atom f : args)) = liftThrows . apply f =<< mapM (eval env) args
eval _ invalid = throwError $
                   BadSpecialForm "Unrecognized special form" invalid

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe
  (throwError $ NotFunction "Unrecognized primitive function args" f)
  ($ args)
  (M.lookup f primitives)

type LispEnvironment = M.Map String ([LispVal] -> ThrowsError LispVal)

primitives :: LispEnvironment
primitives = M.fromList [("+",              numBinop (+)),
                         ("-",              numBinop (-)),
                         ("*",              numBinop (*)),
                         ("/",              numBinop div),
                         ("%",              numBinop mod),
                         ("quotient",       numBinop quot),
                         ("remainder",      numBinop rem),
                         ("boolean?",       isBool),
                         ("number?",        isNum),
                         ("string?",        isString),
                         ("symbol?",        isSym),
                         ("symbol->string", sym2str),
                         ("string->symbol", str2sym),
                         ("not",            lispNot),
                         ("=",              numBoolBinop (==)),
                         ("<",              numBoolBinop (<)),
                         (">",              numBoolBinop (>)),
                         ("/=",             numBoolBinop (/=)),
                         (">=",             numBoolBinop (>=)),
                         ("<=",             numBoolBinop (<=)),
                         ("&&",             boolBoolBinop (&&)),
                         ("||",             boolBoolBinop (||)),
                         ("string=?",       strBoolBinop (==)),
                         ("string<?",       strBoolBinop (<)),
                         ("string>?",       strBoolBinop (>)),
                         ("string<=?",      strBoolBinop (<=)),
                         ("string>=?",      strBoolBinop (>=)),
                         ("car",            car),
                         ("cdr",            cdr),
                         ("cons",           cons),
                         ("eq?",            eqv),
                         ("eqv?",           eqv),
                         ("equal?",         equal)]
  where
    isBool [Bool _] = return $ Bool True
    isBool [_]      = return $ Bool False
    isBool args     = throwError $ NumArgs 1 args

    isNum [Number _] = return $ Bool True
    isNum [_]        = return $ Bool False
    isNum args       = throwError $ NumArgs 1 args

    isString [String _] = return $ Bool True
    isString [_]        = return $ Bool False
    isString args       = throwError $ NumArgs 1 args

    isSym [Atom _] = return $ Bool True
    isSym [_]      = return $ Bool False
    isSym args     = throwError $ NumArgs 1 args

    lispNot [Bool b] = return $ Bool (not b)
    lispNot [x]      = throwError $ TypeMismatch "bool" x
    lispNot args     = throwError $ NumArgs 1 args

    sym2str [Atom s] = return $ String s
    sym2str [x]      = throwError $ TypeMismatch "atom" x
    sym2str args     = throwError $ NumArgs 1 args

    str2sym [String s] = return $ Atom s
    str2sym [x]        = throwError $ TypeMismatch "string" x
    str2sym args       = throwError $ NumArgs 1 args

numBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numBinop _  []       = throwError $ NumArgs 2 []
numBinop _  args@[_] = throwError $ NumArgs 2 args
numBinop op xs       = return . Number . foldl1 op =<< mapM unpackNum xs

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) ->
             [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise        = let left = unpacker $ args !! 0
                           right = unpacker $ args !! 1
                       in Bool <$> (op <$> left <*> right)
  {-| otherwise        = do left <- unpacker $ args !! 0-}
                          {-right <- unpacker $ args !! 1-}
                          {-return . Bool $ left `op` right-}

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] ->
                ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [x]                  = throwError $ TypeMismatch "list" x
car args                 = throwError $ NumArgs 1 args

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [List []]             = throwError $ Default "cdr used on empty list"
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [x]                   = throwError $ TypeMismatch "list" x
cdr args                  = throwError $ NumArgs 1 args

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs]         = return . List $ x:xs
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [xs, y]              = return $ DottedList [xs] y
cons args                 = throwError $ NumArgs 2 args

listEq :: ([a] -> ThrowsError LispVal) -> [a] -> [a] -> Bool
listEq eq xs ys = (length xs == length ys) && (and $ zipWith pairEq xs ys)
  where pairEq x y = case eq [x, y] of
                       Left _ -> False
                       Right (Bool val) -> val
                       Right _ -> error "Error in pairEq"

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool   x, Bool   y] = return . Bool $ x == y
eqv [Number x, Number y] = return . Bool $ x == y
eqv [String x, String y] = return . Bool $ x == y
eqv [Atom   x, Atom   y] = return . Bool $ x == y
eqv [List  xs, List  ys] = return . Bool $ listEq eqv xs ys
eqv [DottedList xs x, DottedList ys y] = eqv [List (x:xs), List (y:ys)]
eqv [_, _] = return $ Bool False
eqv args = throwError $ NumArgs 2 args

data Unpacker = forall a. Eq a => Unpacker (LispVal -> ThrowsError a)

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum x          = throwError $ TypeMismatch "number" x

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr x          = throwError $ TypeMismatch "string" x

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool x        = throwError $ TypeMismatch "bool" x

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals x y (Unpacker unpacker) =
  liftA2 (==) (unpacker x) (unpacker y)
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [List xs, List ys] = return . Bool $ listEq equal xs ys
equal [DottedList xs x, DottedList ys y] = equal [List (x:xs), List (y:ys)]
equal [x, y] = do
  primitiveEquals <- liftM or . mapM (unpackEquals x y) $
                    [Unpacker unpackNum,
                     Unpacker unpackStr,
                     Unpacker unpackBool]
  eqvEquals <- eqv [x, y]
  return . Bool $ primitiveEquals || let (Bool b) = eqvEquals in b
equal args = throwError $ NumArgs 2 args

type Env = M.Map String (IORef LispVal)

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left  err) = throwError err
liftThrows (Right val) = return val

nullEnv :: IO (IORef Env)
nullEnv = newIORef M.empty

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound :: IORef Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . M.member var

getVar :: IORef Env -> String -> IOThrowsError LispVal
getVar envRef var = do
  env <- liftIO . readIORef $ envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (M.lookup var env)

setVar :: IORef Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
  env <- liftIO . readIORef $ envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . flip writeIORef val)
        (M.lookup var env)
  return val

defineVar :: IORef Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
  bound <- liftIO $ isBound envRef var
  if bound then
    setVar envRef var val >> return val
  else
    liftIO $ do
      valRef <- newIORef val
      env <- readIORef envRef
      writeIORef envRef (M.insert var valRef env)
      return val

bindVars :: IORef Env -> [(String, LispVal)] -> IO (IORef Env)
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
  where
    extendEnv env = flip M.union env <$> newBindings
    newBindings = M.fromList <$>
                    forM bindings (\(var, val) -> do
                      ref <- newIORef val
                      return (var, ref))
