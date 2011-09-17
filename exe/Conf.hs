module Main where

import System.Build.ManningBook
import System.Environment

main ::
  IO ()
main =
  do args <- getArgs
     if take 1 args == ["--set-config"]
       then
         do p <- configPath
            writeFile p (show defaultConfig)
       else
         do c <- readConfig
            print c
