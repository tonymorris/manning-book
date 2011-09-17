module Main where

import System.Build.ManningBook
import Control.Monad.Writer
import System.Command

main ::
  IO ()
main =
  do c <- readConfig
     ((_, e), u) <- runWriterT (validate ==>>> c)
     printLog u ==>>> c
     print e
     exitWith e
