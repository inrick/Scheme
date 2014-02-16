{-# LANGUAGE ExistentialQuantification #-}
module Scheme.Eval where

import qualified Data.Map as M
import Control.Applicative ((<$>), (<*>))
import Control.Monad.Error

import Scheme.Error
import Scheme.Data

eval :: LispVal -> ThrowsError LispVal
eval val@(Bool   _) = return val
eval val@(Number _) = return val
eval val@(Float  _) = return val
eval val@(Char   _) = return val
eval val@(String _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", cond, thenBranch, elseBranch]) = do
  result <- eval cond
  case result of
    Bool False -> eval elseBranch
    _          -> eval thenBranch
eval (List (Atom f : args)) = apply f =<< mapM eval args
eval invalid = throwError $ BadSpecialForm "Unrecognized special form" invalid

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

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool   x, Bool   y] = return . Bool $ x == y
eqv [Number x, Number y] = return . Bool $ x == y
eqv [String x, String y] = return . Bool $ x == y
eqv [Atom   x, Atom   y] = return . Bool $ x == y
eqv [List  xs, List  ys] = return . Bool $ (length xs == length ys) &&
                                           (and $ zipWith eqvPair xs ys)
    where
      eqvPair x y = case eqv [x, y] of
                      Left _ -> False
                      Right (Bool val) -> val
                      Right _ -> error "Error in eqvPair"
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
  do ux <- unpacker x
     uy <- unpacker y
     return $ ux == uy
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [x, y] = do
  primitiveEquals <- liftM or . mapM (unpackEquals x y) $
                    [Unpacker unpackNum,
                     Unpacker unpackStr,
                     Unpacker unpackBool]
  eqvEquals <- eqv [x, y]
  return . Bool $ primitiveEquals || let (Bool x) = eqvEquals in x
equal args = throwError $ NumArgs 2 args
