module Scheme.Error where

import Text.ParserCombinators.Parsec (ParseError)
import Control.Monad.Error

import Scheme.Data

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
