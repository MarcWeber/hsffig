module Main where

import Char

main = do p <- getContents 
          let os = map (map ord) $ lines p
          putStrLn $ "module Template where"
          putStrLn $ "import Char"
          putStrLn $ "writeTemplate = mapM putStrLn ["
          mapM (putStrLn . (\s -> "  map chr " ++ s ++ ",") . show) os
          putStrLn $ "  map chr [] ]"

