-- Processing Utilities for a Tokenized C Header File

module ProcHdr where

import C_Lexer
import C_BNF
import Text.ParserCombinators.Parsec
import TokenOps
import qualified Data.Map as Map

-- Extract file name from a TKFILE token

ppFile (TKFILE s p) = Just (read $ (\(a:b:c:ds) -> c) (words s))
ppFile _ = Nothing

-- Get header file name. If a header was preprocessed,
-- its first token will be PPFILE containing the filename
-- for the header to process. The function below makes
-- an attempt to extract such a filename. If the first
-- token in the list is not PPFILE, Nothing is returned.

getHeaderFileName :: [Token] -> Maybe String

getHeaderFileName [] = Nothing
getHeaderFileName (t:ts) = ppFile t

-- Parse the list of tokens.

type TParser a = GenParser Token () a

--

isPpDef (TKDEF _ _) = True
isPpDef _           = False

isCDef (TKDEF _ _)  = False
isCDef (TKFILE _ _) = False
isCDef _            = True

procHeader shs = runParser bnfParser (PState 0 Map.empty) "" $  
  ((ppdefsFrom shs) ++ (cdefsFrom shs)) 
    where ppdefsFrom l = isPpDef `filter` l
          cdefsFrom  l = isCDef `filter` l


