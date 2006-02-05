module HGmain where

import C_Lexer
import ProcHdr
import WriteHsc
import HsfUtils
import System.IO

hsffigMain gcc incls cppopt = do
  hFlush stdout
  s <- getContents
  let shs = scanHeader s
      opts = (map ("-I " ++) incls) ++ cppopt
      gcccmd = intlv (gcc : opts) " "
  case (procHeader shs) of
    Left  y -> error ("Parser error: " ++ (show y))
    Right x -> (do let filename = getHeaderFileName shs
                   writeModHdr filename
                   writeConstAccess (snd x) gcccmd filename
                   writeStructures (fst x) (snd x) filename
                   writeEnums (fst x) (snd x) filename
                   writeStandaloneFunctions (fst x) (snd x) filename)
  hFlush stdout

