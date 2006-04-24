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


Abstract CAS expression.
The CAS expression is an abstraction layer over any maxima-like CAS system.
CAS expression can be converted to maxima lisp and vice versa.
It is the CAS expression with which the user works.
For the moment extension is done through ugly Funcall and the CASExpr itself
is a sum type which is not good for extension since other CAS systems can
provide other functionality. We'll back to this when start constraint solver
or (less possible) Axiom interface.

TODO: redesign CASExpr.
Current troubles with CASExpr:
  - Too much boilerplate code (see below, hm its better to don't)
    It come from:
      - several representations of number (we need exactly one - infinite
        precision numbers)
      - many different cases for binary operations (it doesn't add any syntax
        clearness, but odd to handle)
  - No type checking at all
  - Zero extensibility (funcall is ugly and doesn't grow - it will cause
    too much errors)
  - Too hard to create expressions
    (maybe from string using parser, but again - no type checking)

Thoughts:
  We can create separate types for expressions and define cas functions
  through special interface, e.g.
    solve = eqlist **-> symlist **-> eqlistlist
    diff = expr **-> symbol **-> expr
  But implementation details are not clear for the moment.

> data CASExpr = Integer Integer
>              | Rational Rational
>              | Double Double
>              | Symbol String
>              | String String
>              | Plus CASExpr CASExpr  -- maybe replace this stuff with BinOp?
>              | Minus CASExpr CASExpr
>              | Multiply CASExpr CASExpr
>              | Divide CASExpr CASExpr
>              | Expt CASExpr CASExpr
>              | Equal CASExpr CASExpr
>              | List [CASExpr]
>              | Funcall String [CASExpr] -- its better to exclude this at all!
>                deriving (Eq, Ord, Show)

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

TODO: add CASExpr evaluation
eval :: CASExpr -> CASExpr

TODO: add CASExpr symbol substitution (with evaluation afterwards)

> substitute :: Map.Map String CASExpr -> CASExpr -> CASExpr
> substitute m = subst
>     where subst (Symbol s)     = Map.findWithDefault (Symbol s) s m
>           subst (Plus a b)     = e Plus a b
>           subst (Minus a b)    = e Minus a b
>           subst (Multiply a b) = e Multiply a b
>           subst (Divide a b)   = e Divide a b
>           subst (Expt a b)     = e Expt a b
>           subst (Equal a b)    = e Equal a b
>           subst (List a)       = List $ map subst a
>           subst (Funcall s a)  = Funcall s $ map subst a
>           subst a              = a 
>           e op a b = simpleEval $ op (subst a) (subst b)

simple non-recursive evaluation

> simpleEval :: CASExpr -> CASExpr
> simpleEval = se
>     where se (Plus a b)       = bo (+) (+) (+) a b Plus
>           se (Minus a b)      = bo (-) (-) (-) a b Minus
>           se (Multiply a b)   = bo (*) (*) (*) a b Multiply
>           se (Divide (Integer a) (Integer b)) = Rational (a%b)
>           se (Divide a b)     = bo' (/) (/) a b Divide
>           se (Expt (Integer a)  (Integer b))  =
>               if b >= 0 then Integer $ a ^ b else Rational $ r a ^^ b
>           se (Expt (Integer a)  (Rational b)) = Double $ di a ** dr b
>           se (Expt (Integer a)  (Double b))   = Double $ di a ** b
>           se (Expt (Rational a) (Integer b))  = Rational $ a ^^ b
>           se (Expt (Rational a) (Rational b)) = Double $ dr a ** dr b
>           se (Expt (Rational a) (Double b))   = Double $ dr a ** b
>           se (Expt (Double a)   (Integer b))  = Double $ a ** di b
>           se (Expt (Double a)   (Rational b)) = Double $ a ** dr b
>           se (Expt (Double a)   (Double b))   = Double $ a ** b
>           se (List l)                         = List $ map se l
>           se (Funcall f l)                    = Funcall f $ map se l
>           se a                                = a
>           -- binary operation trying
>           bo iop rop dop (Integer a)  (Integer b)  d = Integer $ iop a b
>           bo iop rop dop a b  d = bo' rop dop a b d 
>           bo' rop dop (Integer a)  (Rational b) d = Rational $ rop (r a) b
>           bo' rop dop (Integer a)  (Double b)   d = Double $ dop (di a) b
>           bo' rop dop (Rational a) (Integer b)  d = Rational $ rop a (r b)
>           bo' rop dop (Rational a) (Rational b) d = Rational $ rop a b
>           bo' rop dop (Rational a) (Double b)   d = Double $ dop (dr a) b
>           bo' rop dop (Double a)   (Integer b)  d = Double $ dop a (di b)
>           bo' rop dop (Double a)   (Rational b) d = Double $ dop a (dr b)
>           bo' rop dop (Double a)   (Double b)   d = Double $ dop a b
>           bo' rop dop a b def                     = def a b
>           r = toRational
>           di = fromRational . toRational
>           dr = fromRational

-- > substitute m (Minus a b)    = substitute m $ Plus a (Multiply (Integer (-1)) b)
-- > substitute m (Multiply a b) = Lisp.List [Lisp.Symbol "MTIMES", substitute m a, substitute m b]
-- > substitute m (Divide a b)   = substitute m $ Multiply a (Expt b (Integer (-1)))
-- > substitute m (Expt a b)     = Lisp.List [Lisp.Symbol "MEXPT", substitute m a, substitute m b]
-- > substitute m (Equal a b)    = Lisp.List [Lisp.Symbol "MEQUAL", substitute m a, substitute m b]
-- > substitute m (List l)       = Lisp.List $ Lisp.Symbol "(MEQUAL)" : map substitute m l
-- > substitute m (Funcall f l)  = Lisp.List $ Lisp.Symbol ('$':f) : map substitute m l
-- > substitute m a = a

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
