{-# LANGUAGE NamedFieldPuns #-}
module Scheme.Data where

import Data.IORef
import Control.Monad.Except
import Control.Monad.Reader
import Text.ParserCombinators.Parsec (ParseError)
import System.IO (Handle)
import qualified Data.Map as M

type Env    = IORef (M.Map String (IORef LispVal))
type LError = Either LispError
type Eval   = ReaderT Env (ExceptT LispError IO)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Char Char
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> LError LispVal)
             | Func { params  :: [String],
                      vararg  :: Maybe String,
                      body    :: [LispVal],
                      closure :: Env }
             | IOFunc ([LispVal] -> Eval LispVal)
             | Port Handle

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

instance Show LispVal where
  show (Atom s)          = s
  show (List xs)         = "(" ++ (unwordsList xs)
                           ++ ")"
  show (DottedList xs y) = "(" ++ show (List xs) ++ " . " ++ show y ++ ")"
  show (Number n)        = show n
  show (Float f)         = show f
  show (Char c)          = show c
  show (String s)        = "\"" ++ s ++ "\""
  show (Bool True)       = "#t"
  show (Bool False)      = "#f"
  show (PrimitiveFunc _) = "<primitive>"
  show (Func {params, vararg}) =
    "(lambda (" ++ unwords (map show params) ++
       (case vararg of
          Nothing  -> ""
          Just arg -> " . " ++ arg) ++ " ...)"
  show (IOFunc _)        = "<IO primitive>"
  show (Port _)          = "<IO port>"

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String LispVal
               | UnboundVar String String
               | Default String

instance Show LispError where
  show (NumArgs expected found) = "Expected " ++ show expected
                               ++ " args; found values "
                               ++ unwordsList found
  show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                    ++ ", found " ++ show found
  show (Parser err) = "Parse error at " ++ show err
  show (BadSpecialForm msg form) = msg ++ ": " ++ show form
  show (UnboundVar msg var) = msg ++ ": " ++ var
  show (NotFunction msg f) = msg ++ ": " ++ show f
  show (Default msg) = msg

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = action `catchError` (return . show)

extractValue :: Either a b -> b
extractValue (Right val) = val
extractValue (Left  _)   = error "Invalid usage of extractValue"
