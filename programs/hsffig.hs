module Main where

import C_Lexer
import ProcHdr
import WriteHsc
import Data.FiniteMap

main = do
  s <- getContents
  let shs = scanHeader s
  case (procHeader shs) of
    Left  y -> error ("Parser error: " ++ (show y))
    Right x -> (do let filename = getHeaderFileName shs
                   writeModHdr filename
                   writeConstAccess (snd x) filename
                   writeStructures (fst x) (snd x) filename
                   writeEnums (fst x) (snd x) filename
                   writeStandaloneFunctions (fst x) (snd x) filename)

