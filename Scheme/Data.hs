{-# LANGUAGE NamedFieldPuns #-}
module Scheme.Data where

import Data.IORef
import Control.Monad.Error
import Text.ParserCombinators.Parsec (ParseError)
import System.IO (Handle)
import qualified Data.Map as M

type LispEnvironment = M.Map String ([LispVal] -> ThrowsError LispVal)

type Env = M.Map String (IORef LispVal)

type IOThrowsError = ErrorT LispError IO

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Char Char
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params  :: [String],
                      vararg  :: Maybe String,
                      body    :: [LispVal],
                      closure :: IORef Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
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
               | NotFunction String String
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

instance Error LispError where
  noMsg  = Default "An error has occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError :: (Show e, MonadError e m) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left  _)   = error "Invalid usage of extractValue"
