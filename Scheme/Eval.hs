{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
module Scheme.Eval where

import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Monad.Error
import Data.IORef
import System.IO

import Scheme.Data
import Scheme.Parser

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
    _          -> throwError $ TypeMismatch "bool" result
eval env (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) =
  makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs env [] body
eval env (List [Atom "load", String filename]) =
  load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
  f <- eval env function
  argVals <- mapM (eval env) args
  apply f argVals
eval _ invalid = throwError $
                   BadSpecialForm "Unrecognized special form" invalid

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc f) args = liftThrows $ f args
apply (Func params vararg body closure) args =
  if nParams /= nArgs && vararg == Nothing then
    throwError $ NumArgs (toInteger nParams) args
  else
    (liftIO . bindVars closure $ zip params args) >>=
    bindVarArgs vararg >>=
    evalBody
  where
    nParams = length params
    nArgs = length args
    remainingArgs = drop nParams args
    evalBody env = liftM last . mapM (eval env) $ body
    bindVarArgs arg env = case arg of
      Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
      Nothing      -> return env
apply (IOFunc f) args = f args
apply _ _ = error "Invalid use of apply"

primBinds :: IO (IORef Env)
primBinds = nullEnv >>=
  (flip bindVars . M.toList $ M.union
     (M.map (\f -> PrimitiveFunc f) $ primitives)
     (M.map (\f -> IOFunc f) $ ioPrimitives))

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
unpackStr (Number n) = return $ show n
unpackStr (Bool   b) = return $ show b
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

-- TODO Rewrite so that bindings is a Map String LispVal
bindVars :: IORef Env -> [(String, LispVal)] -> IO (IORef Env)
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
  where
    extendEnv env = flip M.union env <$> newBindings
    newBindings = M.fromList <$>
                    forM bindings (\(var, val) -> do
                      ref <- newIORef val
                      return (var, ref))

makeFunc :: Maybe String -> IORef Env -> [LispVal] -> [LispVal] ->
            IOThrowsError LispVal
makeFunc varargs env params body = return $
  Func (map show params) varargs body env

makeNormalFunc :: IORef Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> IORef Env -> [LispVal] -> [LispVal] ->
               IOThrowsError LispVal
makeVarargs = makeFunc . Just . show

ioPrimitives :: M.Map String ([LispVal] -> IOThrowsError LispVal)
ioPrimitives = M.fromList [("apply",             applyProc),
                           ("open-input-file",   makePort ReadMode),
                           ("open-output-file",  makePort WriteMode),
                           ("close-input-port",  closePort),
                           ("close-output-port", closePort),
                           ("read",              readProc),
                           ("write",             writeProc),
                           ("read-contents",     readContents),
                           ("read-all",          readAll)]

applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc []                = error "Invalid use of applyProc"

makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port . liftIO $ openFile filename mode
makePort _ _ = error "Invalid use of makePort"

closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> IOThrowsError LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . readExpr
readProc xs = throwError $ NumArgs 1 xs

writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc [] = throwError $ NumArgs 2 []
writeProc xs = throwError $ NumArgs 2 xs

readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String . liftIO . readFile $ filename
readContents [x] = throwError $ TypeMismatch "string" x
readContents xs  = throwError $ NumArgs 1 xs

load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . readExprList

readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll [x] = throwError $ TypeMismatch "string" x
readAll xs  = throwError $ NumArgs 1 xs
