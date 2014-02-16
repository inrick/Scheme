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
                         ("not",            lispNot),
                         ("car",            car),
                         ("cdr",            cdr),
                         ("symbol->string", sym2str),
                         ("string->symbol", str2sym)]
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

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum x          = throwError $ TypeMismatch "number" x

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
