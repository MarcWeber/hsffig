module Main where

import Data.Char
import Control.Monad
import System.Exit
import System.Environment

main = do a <- getArgs
          when ((length a) == 0) $ do
            putStrLn "usage: mktmpl module < infile > outfile"
            exitWith (ExitFailure 1)
          p <- getContents 
          let os = map (map ord) $ lines p
          putStrLn $ "module " ++ head a ++ " where"
          putStrLn $ "import Data.Char"
          putStrLn $ "write" ++ head a ++ " = mapM putStrLn ["
          mapM (putStrLn . (\s -> "  map chr " ++ s ++ ",") . show) os
          putStrLn $ "  map chr [] ]"

