module Main where

import C_Lexer
import ProcHdr
import Data.FiniteMap

main = do
  s <- getContents
  let shs = scanHeader s
  case (procHeader shs) of
    Right x -> (do putStrLn "Parsed Declarations"
                   mapM (putStrLn . show) (fst x)
                   putStrLn "Type Alias Map"
                   mapM (putStrLn . show) (fmToList $ snd x))
    Left  y -> error (show y)

