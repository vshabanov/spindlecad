Interface to Maxima computer algebra system.

> module Maxima where

> import System.IO
> import System.Process
> import Control.Concurrent
> import Control.Concurrent.MVar
> import Control.Exception


> testRun = do
>     (inp,out,err,pid) <- runInteractiveCommand "maxima"
>     endOut <- newEmptyMVar
>     hSetBuffering inp LineBuffering
>     forkIO ((hGetContents out >>= hPutStr stdout) `finally` putMVar endOut ())
>     hPutStrLn inp "to_lisp ();"
>     hPutStrLn inp "(to-maxima)"
>     hPutStrLn inp "quit ();"
>     waitForProcess pid
>     takeMVar endOut -- wait until output is finished
