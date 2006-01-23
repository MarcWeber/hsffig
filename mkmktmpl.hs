module Main where

import Char

main = do p <- getContents 
          let os = map (map ord) $ lines p
          putStrLn $ "module Makefile where"
          putStrLn $ "import Data.Char"
          putStrLn $ "writeMakefile = mapM putStrLn ["
          mapM (putStrLn . (\s -> "  map chr " ++ s ++ ",") . show) os
          putStrLn $ "  map chr [] ]"

