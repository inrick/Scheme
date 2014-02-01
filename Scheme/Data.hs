module Scheme.Data where

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Char Char
             | String String
             | Bool Bool

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
