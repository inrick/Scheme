module Main where

import Data.Char (digitToInt)
import Control.Applicative (liftA, pure, (<$>), (<*>))
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

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> try parseNumber
        <|> try parseBool
        <|> try parseChar
        <|> parseString

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
  (exactness, radix) <- try prefix <|> return (Nothing, Nothing)
  case exactness of
    Just 'i' -> inexact radix
    Just 'e' -> exact radix
    Nothing  -> unspec radix
  where
    exStr = "ei"    -- Possible exactness prefixes
    raStr = "bdox"  -- Possible radix prefixes
    prefix = do
      char '#'
      x <- oneOf $ exStr ++ raStr
      if x `elem` exStr then do
        let exactness = Just x
        radix <- optionMaybe . try $ char '#' >> oneOf raStr
        return (exactness, radix)
      else do
        let radix = Just x
        exactness <- optionMaybe . try $ char '#' >> oneOf exStr
        return (exactness, radix)
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
      n <- many $ oneOf "01"
      let [(m,"")] = readBin n
      return m
        where readBin = readInt 2 (flip elem "01") digitToInt
    parseOct = do
      n <- many1 octDigit
      let [(m,"")] = readOct n
      return m
    parseHex = do
      n <- many1 hexDigit
      let [(m,"")] = readHex n
      return m
    parseFloat = do
      x <- many1 digit
      char '.'
      y <- many1 digit
      let [(f,"")] = readFloat $ x ++ "." ++ y
      return f

parseChar :: Parser LispVal
parseChar = do
  char '#'
  char '\\'
  Char <$> ((string "space" >> return ' ')
             <|> (string "newline" >> return '\n')
             <|> anyChar)

parseString :: Parser LispVal
parseString = do
  char '"'
  s <- many $ noneOf "\"\\" <|> parseEscapeSeq
  char '"'
  return $ String s
  where
    parseEscapeSeq = char '\\'
                  >> (char '"'
                  <|> char '\\'
                  <|> (char 'n' >> return '\n')
                  <|> (char 'r' >> return '\r')
                  <|> (char 't' >> return '\t'))
