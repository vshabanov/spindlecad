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

Interface to Maxima computer algebra system.

> module Maxima (
>     Interpreter,
>     withInterpreter,
>     eval,
>     debugEval -- same as eval but prints Maxima I/O
>   ) where

> import Prelude hiding (catch)
> import System.IO
> import System.Process
> import Control.Monad
> import Control.Exception hiding (try)
> import Text.ParserCombinators.Parsec
> import Data.Ratio
> import Data.Char
> import Data.IORef
> import qualified Data.Map as Map
> import qualified Lisp
> import CASExpr hiding (eval)
> import qualified Data.ByteString.Char8 as B

Type of Maxima command line interpreter.

> type Interpreter = (Handle, IORef String, Handle, ProcessHandle)

Its a simple tuple consisting of
(stdin, stdout contents reference, stderr, ProcessHandle)
As you can see stdout contents (hGetContents stdout) made available
as reference (which contains unread (unparsed) part of output).
The work with this IORef done through parseRef function
which calls specified parser and updates reference so it contains
unparsed part.


withInterpreter utility

> withInterpreter :: (Interpreter -> IO a) -> IO a
> withInterpreter = bracket openInterpreter closeInterpreter

CAS expression evaluation

> eval :: Interpreter -> CASExpr -> IO CASExpr
> eval i e = do sendCAS i e
>               r <- receiveCAS i
>               return r

> debugEval :: Interpreter -> CASExpr -> IO CASExpr
> debugEval i e = do putStrLn "--------- To Maxima ---------"
>                    B.putStrLn $ Lisp.bshow $ toLisp e
>                    sendCAS i e
>                    a <- receiveAnswer i
>                    putStrLn "-------- From Maxima --------"
>                    putStrLn a
>                    putStrLn "-----------------------------"
>                    return $ toCAS $ parseLispAnswer a

For simple test case you can look at the bottom of the file.


Interpreter I/O.

openInterpreter starts maxima session and turn it onto lisp mode.

> openInterpreter :: IO Interpreter
> openInterpreter = do
>     (inp,out,err,pid) <- runInteractiveCommand "maxima"
>     -- TODO: is there any method to determine that process is really running
>     hSetBuffering inp LineBuffering
>     hSetBuffering out LineBuffering
>     hPutStrLn inp "to_lisp ();"
>     outContents <- hGetContents out
>     outContentsRef <- newIORef outContents
>     -- TODO: we need some timeout to wait for correct answer
>     -- and exit when no answer
>     skip <- parseRef skipUntilMaximaPrompt "maxima output" outContentsRef
>     case skip of
>         Left err -> do terminateProcess pid `catch` (\ _ -> return ())
>                        error ("Can't turn maxima into lisp mode: "
>                               ++ show err)
>         Right _ -> return (inp, outContentsRef, err, pid)

closeInterpreter quits maxima

> closeInterpreter :: Interpreter -> IO ()
> closeInterpreter (inp, _, _, pid) = do
>     hPutStrLn inp "to_lisp ();" -- after some errors interpreter
>                                 -- can go back to maxima mode
>     hPutStrLn inp "($quit)"
>     waitForProcess pid
>     return ()

String command sender.
Sends string to maxima interpreter and appends newline

> sendCommand :: Interpreter -> String -> IO ()
> sendCommand (inp,_,_,_) cmd = hPutStrLn inp cmd

> bsendCommand :: Interpreter -> B.ByteString -> IO ()
> bsendCommand (inp,_,_,_) cmd = B.hPut inp cmd >> hPutStrLn inp ""

String answer receiver.
Return string between last answer and MAXIMA> prompt,
i.e. maxima output between two "MAXIMA>" lines.

> receiveAnswer :: Interpreter -> IO String
> receiveAnswer i = do
>     r <- parseOutput (answer "") "maxima output" i
>     case r of
>         Left e -> error ("Incorrect maxima answer:" ++ e)
>         Right a -> return a
>   where answer acc =
>               -- try (do string "MAXIMA>" <?> "Maxima prompt (\"MAXIMA>\")"
>               --         return $ Right $ reverse acc)
>               -- <|> try (do string "(%i"
>               --             many1 digit
>               --             string ")"
>               --             return $ Left $ reverse acc)
>               -- <|> (do c <- anyChar
>               --         answer (c:acc))
>               -- The code below is more fast variant of the code above.
>               -- TODO: we must use Alex & Happy for lexing and parsing
>               --       of lisp values & maxima output
>               do c <- anyChar
>                  if c == '('
>                   then
>                     do c2 <- anyChar
>                        if c2 == '%'
>                          then (do char 'i'
>                                   many1 digit
>                                   char ')'
>                                   return $ Left $ reverse acc)
>                          else  answer (c2:c:acc)
>                   else if c == 'M'
>                    then
>                     do c2 <- anyChar 
>                        if c2 == 'A' then
>                           try (do string "XIMA>"
>                                   return $ Right $ reverse acc)
>                           <|>
>                           answer (c2:c:acc)
>                         else answer (c2:c:acc)
>                    else
>                     answer (c:acc)


Lisp command sender.

> sendLisp :: Interpreter -> Lisp.Value -> IO ()
> --sendLisp i l = bsendCommand i (Lisp.bshow l)
> sendLisp (inp,_,_,_) l = Lisp.hPutLisp inp l >> hPutStrLn inp ""

Lisp answer receiver.
Since there can be non-lisp strings in answer
(for exapmle "`rat' replaced 2.0 by 2//1 = 2.0") we parse any garbage
until lisp value found. After this we continue parsing and return
only last lisp value (since log messages can contain digits and
other stuff that can be misinterpreted as lisp value).
When no lisp value found error raised.

> receiveLisp :: Interpreter -> IO Lisp.Value
> receiveLisp i = do
>     answer <- receiveAnswer i
>     return $ parseLispAnswer answer

> parseLispAnswer :: String -> Lisp.Value
> parseLispAnswer answer =
>     case parse (lispAnswer Nothing) "maxima lisp output" answer of
>         Left err -> error ("Can't parse maxima lisp answer: "
>                            ++ show err)
>         Right (Left e)  -> error ("Lisp error found: " ++ e)
>         Right (Right Nothing)  -> error ("No lisp found in maxima answer:"
>                                          ++ answer)
>         Right (Right (Just r)) -> r
>   where lispAnswer r = 
>                        try (do string "Lisp error"
>                                return (Left answer))
>                        <|> try (do e <- Lisp.parseExpr
>                                    lispAnswer (Just e))
>                        <|> (anyChar >> lispAnswer r)
>                        <|> return (Right r)

CAS command sender.

> sendCAS :: Interpreter -> CASExpr -> IO ()
> --sendCAS i c = sendCommand i (show $ toLisp c) -- 6.0% in profinling
> sendCAS i c = sendLisp i (toLisp c)             -- 4.5% --//--

CAS expression receiver.

> receiveCAS :: Interpreter -> IO CASExpr
> receiveCAS i = do
>     l <- receiveLisp i
>     return $ toCAS l

Auxiliary functons for interpreter I/O.

parseRef works like Parsec.parse but take IORef [tok] instead
of [tok]. After successful parsing done IORef updated to contain
unparsed tokens list.

> parseRef :: GenParser tok () a -> SourceName -> IORef [tok] ->
>             IO (Either ParseError a)
> parseRef parser sourceName tokensRef = do
>     tokens <- readIORef tokensRef
>     case parse (do r <- parser
>                    rest <- getInput
>                    return (r, rest)) sourceName tokens of
>         Left err -> return $ Left err
>         Right (r, rest) -> do writeIORef tokensRef rest
>                               return (Right r)

> parseOutput :: GenParser Char () a -> SourceName -> Interpreter -> IO a
> parseOutput parser sourceName (_, tokensRef, _, _) = do
>     out <- parseRef parser sourceName tokensRef
>     case out of
>         Left err -> error ("Can't parse maxima output: "
>                            ++ show err)
>         Right o  -> return o

Parser that simply skips all input data until first "MAXIMA>" prompt.
Remark that there is no space after '>' since it causes getInput
in parseRef to wait until there is any char after

> skipUntilMaximaPrompt :: Parser ()
> skipUntilMaximaPrompt =
>     try (do string "MAXIMA>" <?> "Maxima prompt (\"MAXIMA>\")"
>             return ())
>     <|> (anyChar >> skipUntilMaximaPrompt)


CASExpr => Lisp.Value converter

TODO: 
Add support for SEC, COSEC (they are returned for diff(tan(x))).
(or maybe add userfun to CASFunction ???
but it will then work with Maxima.eval and will not with CASExpr.eval -
that's too dangerous!!!)

> toLisp :: CASExpr -> Lisp.Value
> toLisp (Integer i)    = Lisp.Integer i
> toLisp (Rational r)   = toLisp (Integer (numerator r)
>                                 `Divide` Integer (denominator r))
> toLisp (Symbol s)     = Lisp.Quote $
>                         if      s == cas_pi then Lisp.Symbol "$%PI"
>                         else if s == cas_e  then Lisp.Symbol "$%E"
>                         else Lisp.Symbol $ symbolToMaxima s
> toLisp (String s)     = Lisp.String s
> toLisp (Plus a b)     = mfuncall "mplus" a b
> toLisp (Minus a b)    = toLisp $ Plus a (Multiply (Integer (-1)) b)
> toLisp (Multiply a b) = mfuncall "mtimes" a b
> toLisp (Divide a b)   = toLisp $ Multiply a (Expt b (Integer (-1)))
> toLisp (Expt a b)     = mfuncall "mexpt" a b
> toLisp (Equal a b)    = mfuncall "mequal" a b
> toLisp (List l)       = mfuncallList "mlist" l
> toLisp (Funcall f l)  =
>     mfuncallList (Map.findWithDefault
>                     (error ("Maxima function not found for: " ++ show f))
>                     f (Map.fromList maximaFunctions)) l

Lisp.Value => CASExpr converter

> toCAS :: Lisp.Value -> CASExpr
> toCAS (Lisp.Integer i)        = Integer i
> toCAS (Lisp.Rational r)       = Rational r
> toCAS (Lisp.Double d)         = -- Rational $ toRational d
>     error "Double values from maxima are forbidden for precision reasons"
> toCAS (Lisp.Symbol s)         = Symbol $ if      s == "$%PI" then cas_pi
>                                          else if s == "$%E"  then cas_e
>                                          else symbolFromMaxima s
> toCAS (Lisp.String s)         = String s
> toCAS (Lisp.Quote q)          = toCAS q
> toCAS (Lisp.AntiQuote q)      = toCAS q
> toCAS (Lisp.List l)           = convert $ map toCAS l
>     where convert (List [Symbol "MLIST"]:xs) = List xs
>           convert (List (Symbol "MPLUS":_):xs) = foldBinOp Plus xs
>           convert (List (Symbol "MTIMES":_):xs) = foldBinOp Multiply xs
>           convert (List (Symbol "MEXPT":_):xs) = foldBinOp Expt xs
>           convert (List (Symbol "MEQUAL":_):xs) = foldBinOp Equal xs
>           convert (List (Symbol "RAT":_):Integer a:Integer b:[]) =
>               Rational (a%b)
>           convert (List (Symbol f:_):xs) =
>               Funcall (Map.findWithDefault
>                          (error ("CAS function not found for: " ++ f))
>                          f (Map.fromList casFunctions)) xs
>           convert a = List a
>           foldBinOp f [] = error "foldBinOp called on empty list"
>           foldBinOp f [x] = x
>           foldBinOp f (x:xs) = compact $ f x (foldBinOp f xs)
>           -- The function below compacts maxima expressions:
>           -- maxima doesn't have Divide & Minus
>           -- it express them in terms of Plus & Multiply
>           compact :: CASExpr -> CASExpr
>           compact (Plus (Multiply (Integer (-1)) a) b) = Minus b a
>           compact (Plus a (Multiply (Integer (-1)) b)) = Minus a b
>           compact (Multiply (Expt a (Integer (-1))) b) = Divide b a
>           compact (Multiply a (Expt b (Integer (-1)))) = Divide a b
>           compact a = a


Some utility functions used by toLisp/toCAS.

CASFunction <=> Maxima symbol association lists.
Remark about '$' and '%' prefixes:
 - (mfuncall `$function params) returns numerical result for all floating
   point functions and normal restult for solve, diff, etc
 - (mfuncall `%function params) returns symbolic result for all floating
   point functions and also symbolic restult for solve, diff, etc
   i.e. it not performs diff, but instead returns ((%DIFF SIMP)...)

> maximaFunctions :: [(CASFunction, String)]
> maximaFunctions = [(CFSolve   , "$solve"),
>                    (CFDiff    , "$diff"),
>                    (CFSubst   , "$sublis"),
>                    (CFAbs     , "mabs"),
>                    (CFSignum  , "%signum"),
>                    (CFExp     , "%exp"),
>                    (CFLog     , "%log"),
>                    (CFSqrt    , "%sqrt"),
>                    (CFSin     , "%sin"),
>                    (CFCos     , "%cos"),
>                    (CFTan     , "%tan"),
>                    (CFASin    , "%asin"),
>                    (CFACos    , "%acos"),
>                    (CFATan    , "%atan"),
>                    (CFSinh    , "%sinh"),
>                    (CFCosh    , "%cosh"),
>                    (CFTanh    , "%tanh"),
>                    (CFASinh   , "%asinh"),
>                    (CFACosh   , "%acosh"),
>                    (CFATanh   , "%atanh")]

> casFunctions :: [(String, CASFunction)]
> casFunctions = map (\(a,b) -> (map toUpper b,a))
>                (maximaFunctions
>                 ++
>                 [(CFSolve, "%solve"),
>                  (CFDiff,  "%diff"),
>                  (CFSubst, "%sublis")])

Utility for using (mfuncall `function ...)

> mfuncall :: String -> CASExpr -> CASExpr -> Lisp.Value
> mfuncall s a b = Lisp.List [Lisp.Symbol "mfuncall",
>                             Lisp.Quote $ Lisp.Symbol s,
>                             toLisp a, toLisp b]

> mfuncallList :: String -> [CASExpr] -> Lisp.Value
> mfuncallList s l = Lisp.List ([Lisp.Symbol "mfuncall",
>                                Lisp.Quote $ Lisp.Symbol s] ++ map toLisp l)

Symbol <=> Maxima symbol conversion routines.

> symbolToMaxima :: String -> String
> symbolToMaxima s =
>     if      map toLower s == s then '$' : map toUpper s
>     else if map toUpper s == s then "|$" ++ map toLower s ++ "|"
>     else "|$" ++ s ++ "|"

> symbolFromMaxima :: String -> String
> symbolFromMaxima ('$':xs) = map toLower xs
> symbolFromMaxima ('|':'$':xs) =
>     if      map toLower s == s then map toUpper s
>     else if map toUpper s == s then map toLower s
>     else s
>   where s = takeWhile (/= '|') xs
> symbolFromMaxima a = a


Simple test case

> testCase = withInterpreter $ \i -> do
>     a <- eval i (subst [("x", Integer 111)] $
>                  solve [diffn (Expt (Symbol "x") (Integer 3)) "x" 1
>                         `Equal` Integer (-12),
>                         (Symbol "x" `Plus` Symbol "y") `Equal` Integer 10
>                        ]
>                  ["x", "y"])
>     print $ substitute (Map.fromList [("A", Integer 4),
>                                       ("B", Integer 2),
>                                       ("C", Integer 1)]) a
