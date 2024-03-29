module Scheme.Parser (readExpr, readExprList) where

import Data.Char (digitToInt)
import Control.Applicative ((<$>), (<*>), (*>))
import Control.Monad.Except
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric (readFloat, readHex, readOct, readInt)

import Scheme.Data

symbol :: Parser Char
symbol = oneOf "!$&|*+-/:<=?>@^_~"

readExpr :: String -> LError LispVal
readExpr = tryRead parseExpr

readExprList :: String -> LError [LispVal]
readExprList = tryRead $ parseExpr `endBy` spaces

tryRead :: Parser a -> String -> LError a
tryRead parser input =
  case parse parser "lisp" input of
    Left  err -> throwError . Parser $ err
    Right val -> return val

spaces :: Parser ()
spaces = skipMany1 space

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
                            _   -> error "Cannot happen"

-- TODO implement more numeric types
parseNumber :: Parser LispVal
parseNumber = do
  (exactness, radix) <- prefix
  case exactness of
    Just 'i' -> inexact radix
    Just 'e' -> exact radix
    Nothing  -> unspec radix
    Just _   -> error "cannot happen"
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
      | otherwise     = error $ "exactRadix called with: " ++ show r

    inexactRadix r
      | r == Nothing ||
        r == Just 'd' = parseFloat
      | otherwise     = error "Invalid radix for inexact numbers"  -- TODO

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
