module Main where

import C_Lexer
import ProcHdr
import WriteXml

main = do
  s <- getContents
  let shs = scanHeader s
  case (procHeader shs) of
    Left  y -> error ("Parser error: " ++ (show y))
    Right x -> (do let filename = getHeaderFileName shs
                   writeXml x filename)
