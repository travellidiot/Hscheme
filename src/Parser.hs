module Parser where

import Text.ParserCombinators.Parsec
import Data.Maybe
import Control.Monad.Except
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Data.Array

import Ast

escapedChars :: Parser Char
escapedChars = do
    _ <- char '\\'
    x <- oneOf "\\\"nrt"
    return $ case x of
        '\\' -> x
        '"' -> x
        'n' -> '\n'
        'r' -> '\r'
        't' -> '\t'


parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ escapedChars <|> noneOf "\"\\"
    _ <- char '"'
    return $ String x

parseCharacter :: Parser LispVal
parseCharacter = do
    try $ string "#\\"
    value <- try (string "space" <|> string "newline" <|> string "unspecified")
        <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
    return $ Character $ case value of
        "space" -> ' '
        "newline" -> '\n'
        _ -> head value

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

oct2dig :: (Num a, Eq a) => String -> a
oct2dig x = fst $ head $ readOct x

hex2dig :: (Num a, Eq a) => String -> a
hex2dig x = fst $ head $ readHex x

bin2dig :: String -> Integer
bin2dig  = bin2dig' 0

bin2dig' :: Num a => a -> String -> a
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs

parseDigitMethod pref digitMethod cvrt = do
    try $ string pref
    x <- many1 digitMethod
    return $ (Number . cvrt) x

parseFloat :: Parser LispVal
parseFloat = do
    int <- many digit
    _ <- char '.'
    frac <- many1 digit
    let realint = case int of "" -> "0"
                              _ -> int
    return $ Float (fst.head.readFloat $ realint++"."++frac)

parseNumber :: Parser LispVal
parseNumber = parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
    where parseDigital2 = parseDigitMethod "#d" digit read
          parseHex = parseDigitMethod "#x" hexDigit hex2dig
          parseOct = parseDigitMethod "#o" octDigit oct2dig
          parseBin = parseDigitMethod "#b" (oneOf "01") bin2dig

parseDigital1 :: Parser LispVal
parseDigital1 = liftM (Number . read) $ many1 digit


parseRatio :: Parser LispVal
parseRatio = do
    num <- many1 digit
    _ <- char '/'
    don <- many1 digit
    return $ Ratio (read num % read don)

parseComplex :: Parser LispVal
parseComplex = let toDouble (Float f) = f
                   toDouble (Number n) = fromIntegral n
               in do
                x <- try parseFloat <|> parseNumber
                _ <- char '+'
                y <- try parseFloat <|> parseNumber
                _ <- char 'i'
                return $ Complex (toDouble x :+ toDouble y)

parseBool :: Parser LispVal
parseBool = do
    _ <- char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces1

parseAnyList :: Parser LispVal
parseAnyList = do
    _ <- char '('
    spaces
    head_ <- sepEndBy parseExpr spaces
    tail_ <- (char '.' >> spaces >> parseExpr) <|> return Nil
    spaces
    _ <- char ')'
    return $ case tail_ of
        Nil -> List head_
        _ -> DottedList head_ tail_


-- parseList :: Parser LispVal
-- parseList = between beg end parseList1
--            where beg = char '(' >> skipMany space
--                  end = skipMany space >> char ')'

-- parseList1 :: Parser LispVal
-- parseList1 = do list <- sepEndBy parseExpr spaces1
--                 datum <- option Nil (char '.' >> spaces1 >> parseExpr)
--                 return $ case datum of
--                     Nil -> List list
--                     val -> DottedList list val


parseDottedList :: Parser LispVal
parseDottedList = do
    first <- endBy parseExpr spaces1
    rest <- char '.' >> spaces1 >> parseExpr
    return $ DottedList first rest

parseQuoted :: Parser LispVal
parseQuoted = do
    _ <- char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do
    _ <- char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
    _ <- char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do
    arrayValues <- sepBy parseExpr spaces1
    return $ Vector (listArray (0, length arrayValues - 1) arrayValues)

parseExpr :: Parser LispVal
parseExpr = parseAtom
        <|> parseString
        <|> try parseComplex
        <|> try parseFloat
        <|> try parseRatio
        <|> try parseNumber
        <|> try parseBool
        <|> try parseCharacter
        <|> try parseQuoted
        <|> try parseQuasiquote
        <|> try parseUnquote
        <|> try parseAnyList
        <|> try (do _ <- string "#("
                    x <- parseVector
                    _ <- char ')'
                    return x)


symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces1 :: Parser ()
spaces1 = skipMany1 $ space <|> escapedChars
