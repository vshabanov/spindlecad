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
> import Data.IORef
> import qualified Data.Map as Map
> import qualified Lisp
> import CASExpr

Type of Maxima command line interpreter.

> type Interpreter = (Handle, IORef String, Handle, ProcessHandle)

Its a simple tuple consisting of
(stdin, stdout contents reference, stderr, ProcessHandle)
As you can see stdout contents (hGetContents stdout) made available
as reference (which contains unread (unparsed) part of output).
The work with this IORef done through parseRef function
which calls specified parser and updates reference so is contains
unparsed part.


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
>     hPutStrLn inp "($quit)"
>     waitForProcess pid
>     return ()

String command sender.
Sends string to maxima interpreter and appends newline

> sendCommand :: Interpreter -> String -> IO ()
> sendCommand (inp,_,_,_) cmd = hPutStrLn inp cmd

String answer receiver.
Return string between last answer and MAXIMA> prompt,
i.e. maxima output between two "MAXIMA>" lines.

> receiveAnswer :: Interpreter -> IO String
> receiveAnswer i = parseOutput (answer "") "maxima output" i
>     where answer acc =
>               try (do string "MAXIMA>" <?> "Maxima prompt (\"MAXIMA>\")"
>                       return $ reverse acc)
>               <|> (do c <- anyChar
>                       answer (c:acc))

Lisp command sender.

> sendLisp :: Interpreter -> Lisp.Value -> IO ()
> sendLisp i l = sendCommand i (show l)

Lisp answer receiver.
Since there can be non-lisp strings in answer
(for exapmle "`rat' replaced 2.0 by 2//1 = 2.0") we parse any garbage
until lisp value found. After this we continue parsing and return
only last lisp value (since log massages can contain digits and
other stuff that can be misinterpreted as lisp value).
When no lisp value found Nothing returned.

> receiveLisp :: Interpreter -> IO (Maybe Lisp.Value)
> receiveLisp i = do
>     answer <- receiveAnswer i
>     case parse (lispAnswer Nothing) "maxima lisp output" answer of
>         Left err -> error ("Can't parse maxima lisp answer: "
>                            ++ show err)
>         Right r  -> return r
>   where lispAnswer r = try (do e <- Lisp.parseExpr
>                                lispAnswer (Just e))
>                        <|> (anyChar >> lispAnswer r)
>                        <|> return r

CAS command sender.

> sendCAS :: Interpreter -> CASExpr -> IO ()
> sendCAS i c = sendCommand i (show $ toLisp c)

CAS expression receiver.

> receiveCAS :: Interpreter -> IO (Maybe CASExpr)
> receiveCAS i = do
>     l <- receiveLisp i
>     case l of
>         Nothing -> return Nothing
>         Just l  -> return $ Just (toCAS l)

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

> toLisp :: CASExpr -> Lisp.Value
> toLisp (Integer i)    = Lisp.Integer i
> toLisp (Rational r)   = Lisp.Rational r
> toLisp (Double d)     = Lisp.Double d
> toLisp (Symbol s)     = Lisp.Symbol ("`$" ++ s)
> toLisp (String s)     = Lisp.String s
> toLisp (Plus a b)     = Lisp.List [Lisp.Symbol "MPLUS", toLisp a, toLisp b]
> toLisp (Minus a b)    = toLisp $ Plus a (Multiply (Integer (-1)) b)
> toLisp (Multiply a b) = Lisp.List [Lisp.Symbol "MTIMES", toLisp a, toLisp b]
> toLisp (Divide a b)   = toLisp $ Multiply a (Expt b (Integer (-1)))
> toLisp (Expt a b)     = Lisp.List [Lisp.Symbol "MEXPT", toLisp a, toLisp b]
> toLisp (Equal a b)    = Lisp.List [Lisp.Symbol "MEQUAL", toLisp a, toLisp b]
> toLisp (List l)       = Lisp.List $ Lisp.Symbol "(MEQUAL)" : map toLisp l
> toLisp (Funcall f l)  = Lisp.List $ Lisp.Symbol ('$':f) : map toLisp l

Lisp.Value => CASExpr converter

> toCAS :: Lisp.Value -> CASExpr
> toCAS (Lisp.Integer i)        = Integer i
> toCAS (Lisp.Rational r)       = Rational r
> toCAS (Lisp.Double d)         = Double d
> toCAS (Lisp.Symbol s)         = Symbol (case s of
>                                           '`':'$':s -> s
>                                           '$':s     -> s
>                                           otherwise -> s)
> toCAS (Lisp.String s)         = String s
> toCAS (Lisp.Quote q)          = toCAS q
> toCAS (Lisp.List l)           = convert $ map toCAS l
>     where convert (List [Symbol "MLIST"]:xs) = List xs
>           convert (List (Symbol "MPLUS":_):xs) = foldBinOp Plus xs
>           convert (List (Symbol "MTIMES":_):xs) = foldBinOp Multiply xs
>           convert (List (Symbol "MEXPT":_):xs) = foldBinOp Expt xs
>           convert (List (Symbol "MEQUAL":_):xs) = foldBinOp Equal xs
>           convert (List (Symbol "RAT":_):Integer a:Integer b:[]) =
>               Rational (a%b)
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

> testCase = do
>     i <- openInterpreter
>     sendCommand i "($solve #$(a*x^2+2*b*x+c=0)$ #$(x)$)"
>     a <- receiveCAS i
>     case a of
>         Just a -> print $ substitute (Map.fromList [("A", Integer 4),
>                                                     ("B", Integer 2),
>                                                     ("C", Integer 1)]) a
>         Nothing -> return ()
>     closeInterpreter i

-------------------------------------------------------------------------------

Time tests.
Machine: AMD Athlon XP 1700+, 1GB RAM
System: Maxima 5.9.2 (GNU Common Lisp (GCL) 2.6.7)
Method: mapM_ <test code> [1..1000]  ghc 6.4 interactive (interpreted) mode

Warning !!!
This times are incorrect, ghci (:set +s option) return only time
in ghci itself ignoring maxima time. Retry it with PerformanceTest.

maxima run (when in cache):

    do { i <- openInterpreter; closeInterpreter i }             ==> ~5.1 msec

equation solving (I/O only):

    do sendCommand i "($solve #$(a*x^2+2*b*x+c=0)$ #$(x)$)"     ==> ~5.2 msec
       a <- receiveAnswer i
       return a

equation solving (I/O + lisp answer parsing):

    do sendCommand i "($solve #$(a*x^2+2*b*x+c=0)$ #$(x)$)"     ==> ~5 msec
       a <- receiveAnswer i
       let Right e = parse (do { optional spaces
                                 e <- parseExpr
                                 optional spaces
                                 return e}) "" a in return e;   ==>
