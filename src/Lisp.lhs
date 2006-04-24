Lisp value and lisp value I/O

> module Lisp where

> import Control.Monad
> import Text.ParserCombinators.Parsec
> import Data.Ratio


Lisp values data type

> data Value = Integer Integer
>            | Rational Rational
>            | Double Double
>            | Symbol String
>            | String String
>            | List [Value]
>            | Quote Value deriving (Eq, Ord)


Show instance for lisp values

> instance Show Value where
>     show (Integer i) = show i
>     show (Rational r) = show (numerator r) ++ '/' : show (denominator r)
>     show (Double d) = show d
>     show (Symbol s) = s
>     show (String s) = show s
>     show (List l) = "(" ++ unwords (map show l) ++ ")"
>     show (Quote v) = '`' : show v


Parser for lisp values

> symbol :: Parser Char
> symbol = oneOf "!$%&|*+-/:<=>?@^_~"

> integer :: Parser String
> integer = do char '-'
>              digits <- many1 digit
>              return $ '-' : digits
>           <|>
>           (optional (char '+') >> many1 digit)

> parseInteger :: Parser Value
> parseInteger = liftM (Integer . read) $ integer

> parseRational :: Parser Value
> parseRational = liftM (Rational . read) $
>                 do num <- integer
>                    char '/'
>                    den <- many1 digit
>                    return $ num ++ "%" ++ den

> parseDouble :: Parser Value
> parseDouble = liftM (Double . read) $
>               do i1 <- integer
>                  rest <- try (do p <- point
>                                  e <- exp
>                                  return $ p ++ e)
>                          <|> point
>                          <|> exp
>                  return $ i1 ++ rest
>               where
>                  point = do char '.'
>                             digits <- option [] (many1 digit)
>                             return $ '.' : (if digits == []
>                                               then "0"
>                                               else digits)
>                  exp = do char 'e' <|> char 'E'
>                           exp <- integer
>                           return $ 'e' : exp

> parseSymbol :: Parser Value
> parseSymbol = do first <- letter <|> symbol
>                  rest <- many (letter <|> digit <|> symbol)
>                  return $ Symbol (first : rest)

> parseString :: Parser Value
> parseString = do char '"'
>                  x <- many (noneOf "\"")
>                  char '"'
>                  return $ String x

> parseExpr :: Parser Value
> parseExpr = try parseRational
>         <|> try parseDouble
>         <|> parseInteger
>         <|> parseSymbol
>         <|> parseString
>         <|> do char '('
>                optional spaces
>                l <- endBy parseExpr spaces
>                char ')'
>                return $ List l
>         <|> do char '`'
>                expr <- parseExpr
>                return $ Quote expr
