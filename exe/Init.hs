module Main where

import System.Build.ManningBook
import Control.Monad.Writer
import System.Environment

main ::
  IO ()
main =
  do c <- readConfig
     (_, u) <- runWriterT (spellcheckInit ==>>> c)
     printLog u ==>>> c
     args <- getArgs
     unless (take 1 args == ["--no-dependencies"]) $
            do (_, v) <- runWriterT (allDownload ==>>> c)
               printLog v ==>>> c

