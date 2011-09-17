module Main where

import System.Build.ManningBook
import Control.Monad.Writer
import System.Environment

main ::
  IO ()
main =
  do c <- readConfig
     (_, u) <- runWriterT (rmDistDir ==>>> c)
     printLog u ==>>> c
     args <- getArgs
     when (take 1 args == ["--all"]) $
            do (_, v) <- runWriterT (rmDependencyDir ==>>> c)
               printLog v ==>>> c

