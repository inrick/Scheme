module Scheme.Eval where

import qualified Data.Map as M

import Scheme.Data

type LispEnvironment = M.Map String ([LispVal] -> LispVal)

eval :: LispVal -> LispVal
eval val@(Bool   _) = val
eval val@(Number _) = val
eval val@(Float  _) = val
eval val@(Char   _) = val
eval val@(String _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom f : args)) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ M.lookup f primitives

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
    isBool   [Bool _]      = Bool True
    isBool   _             = Bool False
    isNum    [Number _]    = Bool True
    isNum    _             = Bool False
    isString [String _]    = Bool True
    isString _             = Bool False
    isSym    [Atom _]      = Bool True
    isSym    _             = Bool False
    lispNot  [Bool b]      = Bool (not b)
    lispNot  _             = Bool False
    car      [List (x:_)]  = x
    car      _             = error "error"
    cdr      [List (_:xs)] = List xs
    cdr      _             = error "error"
    sym2str  [Atom s]      = String s
    sym2str  _             = error "error"
    str2sym  [String s]    = Atom s
    str2sym  _             = error "error"

numBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numBinop op = Number . foldl1 op . map unpackNum

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0
