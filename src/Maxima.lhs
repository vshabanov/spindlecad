Interface to Maxima computer algebra system.

> module Maxima where

> import System.IO
> import System.Process
> import Control.Monad
> import Control.Concurrent
> import Control.Concurrent.MVar
> import Control.Exception hiding (try, catch)
> import Text.ParserCombinators.Parsec
> import Data.Ratio

The type of Maxima command line interpreter
Its a simple tuple consisting of
stdin, stdout, stderr, ProcessHandle

> type Interpreter = (Handle, Handle, Handle, ProcessHandle)

openInterpreter starts maxima session and turn it onto lisp mode

> openInterpreter :: IO Interpreter
> openInterpreter = do
>     (inp,out,err,pid) <- runInteractiveCommand "maxima"
>     -- TODO: is there any method to determine that process is really running
>     hSetBuffering inp LineBuffering
>     hSetBuffering out LineBuffering
>     hPutStrLn inp "to_lisp ();"
>     outContents <- hGetContents out
>     -- TODO: we need some timeout to wait for correct answer
>     case parse skipUntilMaximaPrompt "maxima output" outContents of
>         Left err -> do terminateProcess pid `catch` (\ _ -> return ())
>                        error ("Can't turn maxima into lisp mode: "
>                               ++ show err)
>         Right _ -> return (inp,out,err,pid)

closeInterpreter quits maxima

> closeInterpreter :: Interpreter -> IO ()
> closeInterpreter (inp, _, _, pid) = do
>     hPutStrLn inp "($quit)"
>     waitForProcess pid
>     return ()

> skipUntilMaximaPrompt :: Parser ()
> skipUntilMaximaPrompt =
>     try (do string "MAXIMA> " <?> "Maxima prompt (\"MAXIMA>\")"
>             return ())
>     <|> (anyChar >> skipUntilMaximaPrompt)

> sendCommand :: Interpreter -> String -> IO ()
> sendCommand (inp,_,_,_) cmd = hPutStrLn inp cmd


Lisp values data type

> data LispVal = Integer Integer
>              | Rational Rational
>              | Double Double
>              | Symbol String
>              | String String
>              | List [LispVal]
>              | Quote LispVal deriving (Eq, Ord)


Show instance for lisp values

> instance Show LispVal where
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

> parseInteger :: Parser LispVal
> parseInteger = liftM (Integer . read) $ integer

> parseRational :: Parser LispVal
> parseRational = liftM (Rational . read) $
>                 do num <- integer
>                    char '/'
>                    den <- many1 digit
>                    return $ num ++ "%" ++ den

> parseDouble :: Parser LispVal
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

> parseSymbol :: Parser LispVal
> parseSymbol = do first <- letter <|> symbol
>                  rest <- many (letter <|> digit <|> symbol)
>                  return $ Symbol (first : rest)

> parseString :: Parser LispVal
> parseString = do char '"'
>                  x <- many (noneOf "\"")
>                  char '"'
>                  return $ String x

> parseExpr :: Parser LispVal
> parseExpr = try parseRational
>         <|> try parseDouble
>         <|> parseInteger
>         <|> parseSymbol
>         <|> parseString
>         <|> do char '('
>                l <- sepBy parseExpr spaces
>                char ')'
>                return $ List l
>         <|> do char '`'
>                expr <- parseExpr
>                return $ Quote expr
