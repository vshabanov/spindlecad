--
--  Copyright (C) 2006 Vladimir Shabanov
--
--  This file is part of SpindleCAD.
--
--  SpindleCAD is free software; you can redistribute it and/or modify
--  it under the terms of the GNU General Public License as published by
--  the Free Software Foundation; either version 2 of the License, or
--  (at your option) any later version.
--
--  SpindleCAD is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with SpindleCAD; if not, write to the Free Software
--  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
--

Lisp value and lisp value I/O

> module Lisp where

> import Control.Monad
> import Text.ParserCombinators.Parsec
> import Data.Ratio
> import qualified Data.ByteString.Char8 as B


Lisp values data type

> data Value = Integer Integer
>            | Rational Rational
>            | Double Double
>            | Symbol String
>            | String String
>            | List [Value]
>            | Quote Value
>            | AntiQuote Value
>              deriving (Eq, Ord)


Show instance for lisp values

> instance Show Value where
>     show (Integer i) = show i
>     show (Rational r) = show (numerator r) ++ '/' : show (denominator r)
>     show (Double d) = show d
>     show (Symbol s) = s
>     show (String s) = show s
>     show (List l) = "(" ++ unwords (map show l) ++ ")"
>     show (Quote v) = '\'' : show v
>     show (AntiQuote v) = ',' : show v

> bshow :: Value -> B.ByteString
> bshow (Integer i) = B.pack $ show i
> bshow (Rational r) = B.concat [B.pack $ show (numerator r),
>                                B.packChar '/',
>                                B.pack $ show (denominator r)]
> bshow (Double d) = B.pack $ show d
> bshow (Symbol s) = B.pack $ s
> bshow (String s) = B.pack $ show s
> bshow (List l) = B.concat [B.packChar '(',
>                            B.unwords (map bshow l),
>                            B.packChar ')']
> bshow (Quote v) = B.cons '\'' $ bshow v
> bshow (AntiQuote v) = B.cons ',' $ bshow v

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
>         <|> try parseInteger
>         <|> parseSymbol
>         <|> parseString
>         <|> do char '('
>                optional spaces
>                l <- endBy parseExpr spaces
>                char ')'
>                return $ List l
>         <|> do char '\''
>                expr <- parseExpr
>                return $ Quote expr
>         <|> do char ','
>                expr <- parseExpr
>                return $ AntiQuote expr
