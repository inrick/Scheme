module Main where

import Data.Char (digitToInt)
import Control.Applicative (liftA, pure, (<$>), (<*>), (*>))
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readFloat, readHex, readOct, readInt)
import qualified Data.Map as M

main :: IO ()
main = print . eval . readExpr . (!! 0) =<< getArgs

symbol :: Parser Char
symbol = oneOf "!$&|*+-/:<=?>@^_~"

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> String $ "No match: " ++ show err
  Right val -> val

spaces :: Parser ()
spaces = skipMany1 space

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Char Char
             | String String
             | Bool Bool

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

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = DottedList
  <$> endBy parseExpr spaces
  <*> (char '.' *> spaces *> parseExpr)

quote :: Char -> String -> Parser LispVal
quote c atom = do
  _ <- char c
  x <- parseExpr
  return $ List [Atom atom, x]

parseQuoted :: Parser LispVal
parseQuoted = quote '\'' "quote"

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = quote '`' "quasiquote"

parseUnquoted :: Parser LispVal
parseUnquoted = quote ',' "unquote"

parseExpr :: Parser LispVal
parseExpr = parseAtom
        -- Numbers, booleans and characters are all prefixed with #.
        -- Use try to backtrack when needed.
        <|> try parseNumber
        <|> try parseBool
        <|> try parseChar
        <|> parseString
        <|> parseQuoted
        <|> parseQuasiquoted
        <|> parseUnquoted
        <|> between
              (char '(')
              (char ')')
              (try parseList <|> parseDottedList)

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter
       <|> symbol
  rest <- many $ letter
             <|> digit
             <|> symbol
  let atom = first : rest
  return $ Atom atom

parseBool :: Parser LispVal
parseBool = do _ <- char '#'
               b <- oneOf "ft"
               return $ case b of
                            'f' -> Bool False
                            't' -> Bool True

-- TODO implement more numeric types
parseNumber :: Parser LispVal
parseNumber = do
  (exactness, radix) <- prefix
  case exactness of
    Just 'i' -> inexact radix
    Just 'e' -> exact radix
    Nothing  -> unspec radix
  where
    prefix = do
      do exactness <- try parseExactness
         radix     <- maybeRadix
         return (Just exactness, radix)
      <|> do radix     <- maybeRadix
             exactness <- maybeExactness
             return (exactness, radix)

    parseRadix     = char '#' *> oneOf "bdox"  -- Possible radix prefixes
    parseExactness = char '#' *> oneOf "ei"    -- Possible exactness prefixes
    maybeRadix     = optionMaybe . try $ parseRadix
    maybeExactness = optionMaybe . try $ parseExactness

    inexact r = Float  <$> inexactRadix r
    exact   r = Number <$> exactRadix r

    exactRadix r
      | r == Nothing ||
        r == Just 'd' = parseDec
      | r == Just 'b' = parseBin
      | r == Just 'o' = parseOct
      | r == Just 'x' = parseHex

    inexactRadix r
      | r == Nothing ||
        r == Just 'd' = parseFloat
      | otherwise     = error "invalid radix for inexact numbers"  -- TODO

    unspec r  -- When exactness is unspecified
      | r == Nothing ||
        r == Just 'd' = try (inexact r) <|> exact r
      | otherwise     = exact r

    parseDec = read <$> many1 digit

    parseBin = do
      n <- many1 $ oneOf "01"
      let [(m,"")] = readBin n
      return m
        where readBin = readInt 2 (`elem` "01") digitToInt

    parseOct = do
      n <- many1 octDigit
      let [(m,"")] = readOct n
      return m

    parseHex = do
      n <- many1 hexDigit
      let [(m,"")] = readHex n
      return m

    parseFloat = do
      x <- option "0" $ many1 digit
      _ <- char '.'
      y <- many1 digit
      let [(f,"")] = readFloat $ x ++ "." ++ y
      return f

parseChar :: Parser LispVal
parseChar = do
  _ <- char '#'
  _ <- char '\\'
  Char <$> ((string "space" *> return ' ')
             <|> (string "newline" *> return '\n')
             <|> anyChar)

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  s <- many $ noneOf "\"\\" <|> parseEscapeSeq
  _ <- char '"'
  return $ String s
  where
    parseEscapeSeq = char '\\'
                  *> (char '"'
                  <|> char '\\'
                  <|> (char 'n' *> return '\n')
                  <|> (char 'r' *> return '\r')
                  <|> (char 't' *> return '\t'))

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

type LispEnvironment = M.Map String ([LispVal] -> LispVal)

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
