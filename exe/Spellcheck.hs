module Main where

import System.Build.ManningBook
import System.Environment
import System.Command
import Control.Monad.Writer

main ::
  IO ()
main =
  do args <- getArgs
     c <- readConfig
     if take 1 args == ["--non-interactive"]
       then
         do ((_, e), u) <- runWriterT (spellcheckNoninteractive ==>>> c)
            printLog u ==>>> c
            print e
            exitWith e
       else
         do (_, u) <- runWriterT (spellcheck ==>>> c)
            printLog u ==>>> c
            return ()

