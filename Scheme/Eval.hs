{-# LANGUAGE ExistentialQuantification, NamedFieldPuns #-}
module Scheme.Eval where

import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>), liftA2)
import Control.Monad.Error
import Control.Monad.Reader
import Data.IORef
import System.IO

import Scheme.Data
import Scheme.Parser

eval :: LispVal -> Eval LispVal
eval val@(Bool   _) = return val
eval val@(Number _) = return val
eval val@(Float  _) = return val
eval val@(Char   _) = return val
eval val@(String _) = return val
eval (Atom atom) = getVar atom
eval (List [Atom "quote", val]) = return val
eval (List [Atom "set!", Atom var, form]) = eval form >>= setVar var
eval (List [Atom "define", Atom var, form]) = eval form >>= defineVar var
eval (List [Atom "if", cond, thenBranch, elseBranch]) = do
  result <- eval cond
  case result of
    Bool True  -> eval thenBranch
    Bool False -> eval elseBranch
    _          -> throwError $ TypeMismatch "bool" result
eval (List (Atom "define" : List (Atom var : params) : body)) =
  makeNormalFunc params body >>= defineVar var
eval (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
  makeVarargs varargs params body >>= defineVar var
eval (List (Atom "lambda" : List params : body)) = makeNormalFunc params body
eval (List (Atom "lambda" : DottedList params varargs : body)) =
  makeVarargs varargs params body
eval (List (Atom "lambda" : varargs@(Atom _) : body)) =
  makeVarargs varargs [] body
eval (List [Atom "load", String filename]) = do
  file <- load filename
  liftM last . mapM eval $ file
eval (List (function : args)) = do
  f <- eval function
  argVals <- mapM eval args
  apply f argVals
eval invalid = throwError $
                 BadSpecialForm "Unrecognized special form" invalid

apply :: LispVal -> [LispVal] -> Eval LispVal
apply (PrimitiveFunc f) args = liftError $ f args
apply (Func params vararg body closure) args =
  if nParams /= nArgs && vararg == Nothing then
    throwError $ NumArgs (toInteger nParams) args
  else
    do env <- liftIO . bindVars closure $ zip params args
       env' <- bindVarArgs vararg env
       local (const env') evalBody
  where
    nParams = length params
    nArgs = length args
    remainingArgs = drop nParams args
    evalBody = liftM last . mapM eval $ body
    bindVarArgs arg env =
      case arg of
        Just argName -> liftIO $ bindVars env [(argName, List remainingArgs)]
        Nothing      -> return env
apply (IOFunc f) args = f args
apply _ _ = error "Invalid use of apply"

primBinds :: IO Env
primBinds = nullEnv >>=
  (flip bindVars $
     (map (\(s,f) -> (s, PrimitiveFunc f)) $ primitives) ++
     (map (\(s,f) -> (s, IOFunc f)) $ ioPrimitives))

primitives :: [(String, [LispVal] -> LError LispVal)]
primitives = [("+",              numBinop (+)),
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

numBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LError LispVal
numBinop _  []       = throwError $ NumArgs 2 []
numBinop _  args@[_] = throwError $ NumArgs 2 args
numBinop op xs       = return . Number . foldl1 op =<< mapM unpackNum xs

boolBinop :: (LispVal -> LError a) -> (a -> a -> Bool) ->
             [LispVal] -> LError LispVal
boolBinop unpacker op args
  | length args /= 2 = throwError $ NumArgs 2 args
  | otherwise        = let left = unpacker $ args !! 0
                           right = unpacker $ args !! 1
                       in Bool <$> (op <$> left <*> right)
  {-| otherwise        = do left <- unpacker $ args !! 0-}
                          {-right <- unpacker $ args !! 1-}
                          {-return . Bool $ left `op` right-}

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] ->
                LError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> LError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> LError LispVal
boolBoolBinop = boolBinop unpackBool

car :: [LispVal] -> LError LispVal
car [List (x:_)]         = return x
car [DottedList (x:_) _] = return x
car [x]                  = throwError $ TypeMismatch "list" x
car args                 = throwError $ NumArgs 1 args

cdr :: [LispVal] -> LError LispVal
cdr [List (_:xs)]         = return $ List xs
cdr [List []]             = throwError $ Default "cdr used on empty list"
cdr [DottedList [_] x]    = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [x]                   = throwError $ TypeMismatch "list" x
cdr args                  = throwError $ NumArgs 1 args

cons :: [LispVal] -> LError LispVal
cons [x, List xs]         = return . List $ x:xs
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [xs, y]              = return $ DottedList [xs] y
cons args                 = throwError $ NumArgs 2 args

listEq :: ([a] -> LError LispVal) -> [a] -> [a] -> Bool
listEq eq xs ys = (length xs == length ys) && (and $ zipWith pairEq xs ys)
  where pairEq x y = case eq [x, y] of
                       Left _ -> False
                       Right (Bool val) -> val
                       Right _ -> error "Error in pairEq"

eqv :: [LispVal] -> LError LispVal
eqv [Bool   x, Bool   y] = return . Bool $ x == y
eqv [Number x, Number y] = return . Bool $ x == y
eqv [String x, String y] = return . Bool $ x == y
eqv [Atom   x, Atom   y] = return . Bool $ x == y
eqv [List  xs, List  ys] = return . Bool $ listEq eqv xs ys
eqv [DottedList xs x, DottedList ys y] = eqv [List (x:xs), List (y:ys)]
eqv [_, _] = return $ Bool False
eqv args = throwError $ NumArgs 2 args

data Unpacker = forall a. Eq a => Unpacker (LispVal -> LError a)

unpackNum :: LispVal -> LError Integer
unpackNum (Number n) = return n
unpackNum x          = throwError $ TypeMismatch "number" x

unpackStr :: LispVal -> LError String
unpackStr (String s) = return s
unpackStr (Number n) = return $ show n
unpackStr (Bool   b) = return $ show b
unpackStr x          = throwError $ TypeMismatch "string" x

unpackBool :: LispVal -> LError Bool
unpackBool (Bool b) = return b
unpackBool x        = throwError $ TypeMismatch "bool" x

unpackEquals :: LispVal -> LispVal -> Unpacker -> LError Bool
unpackEquals x y (Unpacker unpacker) =
  liftA2 (==) (unpacker x) (unpacker y)
  `catchError` (const $ return False)

equal :: [LispVal] -> LError LispVal
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

liftError :: LError a -> Eval a
liftError (Left  err) = throwError err
liftError (Right val) = return val

nullEnv :: IO Env
nullEnv = newIORef M.empty

runEval :: Env -> Eval String -> IO String
runEval env action = do
  val <- runErrorT . trapError $ runReaderT action env
  return . extractValue $ val

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . M.member var

getVar :: String -> Eval LispVal
getVar var = do
  envRef <- ask
  env <- liftIO . readIORef $ envRef
  maybe (throwError $ UnboundVar "Getting an unbound variable" var)
        (liftIO . readIORef)
        (M.lookup var env)

setVar :: String -> LispVal -> Eval LispVal
setVar var val = do
  envRef <- ask
  env <- liftIO . readIORef $ envRef
  maybe (throwError $ UnboundVar "Setting an unbound variable" var)
        (liftIO . flip writeIORef val)
        (M.lookup var env)
  return val

defineVar :: String -> LispVal -> Eval LispVal
defineVar var val = do
  envRef <- ask
  bound <- liftIO $ isBound envRef var
  if bound then
    setVar var val >> return val
  else
    liftIO $ do
      valRef <- newIORef val
      env <- readIORef envRef
      writeIORef envRef (M.insert var valRef env)
      return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv >>= newIORef
  where
    extendEnv env = flip M.union env <$> newBindings
    newBindings = M.fromList <$>
                    forM bindings (\(var, val) -> do
                      ref <- newIORef val
                      return (var, ref))

makeFunc :: Maybe String -> [LispVal] -> [LispVal] -> Eval LispVal
makeFunc varargs params body =
  return . Func (map show params) varargs body =<< ask

makeNormalFunc :: [LispVal] -> [LispVal] -> Eval LispVal
makeNormalFunc = makeFunc Nothing

makeVarargs :: LispVal -> [LispVal] -> [LispVal] -> Eval LispVal
makeVarargs = makeFunc . Just . show

ioPrimitives :: [(String, [LispVal] -> Eval LispVal)]
ioPrimitives = [("apply",             applyProc),
                ("open-input-file",   makePort ReadMode),
                ("open-output-file",  makePort WriteMode),
                ("close-input-port",  closePort),
                ("close-output-port", closePort),
                ("read",              readProc),
                ("write",             writeProc),
                ("read-contents",     readContents),
                ("read-all",          readAll)]

applyProc :: [LispVal] -> Eval LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc []                = error "Invalid use of applyProc"

makePort :: IOMode -> [LispVal] -> Eval LispVal
makePort mode [String filename] = liftM Port . liftIO $ openFile filename mode
makePort _ _ = error "Invalid use of makePort"

closePort :: [LispVal] -> Eval LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _ = return $ Bool False

readProc :: [LispVal] -> Eval LispVal
readProc [] = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftError . readExpr
readProc xs = throwError $ NumArgs 1 xs

writeProc :: [LispVal] -> Eval LispVal
writeProc [obj] = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc [] = throwError $ NumArgs 2 []
writeProc xs = throwError $ NumArgs 2 xs

readContents :: [LispVal] -> Eval LispVal
readContents [String filename] = liftM String . liftIO . readFile $ filename
readContents [x] = throwError $ TypeMismatch "string" x
readContents xs  = throwError $ NumArgs 1 xs

load :: String -> Eval [LispVal]
load filename = (liftIO $ readFile filename) >>= liftError . readExprList

readAll :: [LispVal] -> Eval LispVal
readAll [String filename] = liftM List $ load filename
readAll [x] = throwError $ TypeMismatch "string" x
readAll xs  = throwError $ NumArgs 1 xs
