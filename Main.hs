module Main where

import Data.Char (digitToInt)
import Control.Applicative (liftA, pure, (<$>), (<*>), (*>))
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readFloat, readHex, readOct, readInt)

main :: IO ()
main = putStrLn . readExpr . (!! 0) =<< getArgs

symbol :: Parser Char
symbol = oneOf "!$&|*+-/:<=?>@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found value: " ++ show val

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Float
             | Char Char
             | String String
             | Bool Bool
  deriving (Show)

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = DottedList
  <$> endBy parseExpr spaces
  <*> (char '.' *> spaces *> parseExpr)

quote :: Char -> String -> Parser LispVal
quote c atom = do
  char c
  x <- parseExpr
  return $ List [Atom atom, x]

parseQuoted      = quote '\'' "quote"
parseQuasiquoted = quote '`'  "quasiquote"
parseUnquoted    = quote ','  "unquote"

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
parseBool = do char '#'
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
      char '.'
      y <- many1 digit
      let [(f,"")] = readFloat $ x ++ "." ++ y
      return f

parseChar :: Parser LispVal
parseChar = do
  char '#'
  char '\\'
  Char <$> ((string "space" *> return ' ')
             <|> (string "newline" *> return '\n')
             <|> anyChar)

parseString :: Parser LispVal
parseString = do
  char '"'
  s <- many $ noneOf "\"\\" <|> parseEscapeSeq
  char '"'
  return $ String s
  where
    parseEscapeSeq = char '\\'
                  *> (char '"'
                  <|> char '\\'
                  <|> (char 'n' *> return '\n')
                  <|> (char 'r' *> return '\r')
                  <|> (char 't' *> return '\t'))
