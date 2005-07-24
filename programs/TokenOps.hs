-- Primitive Token Operations

module TokenOps where

import C_Lexer
import Parsec
import ParsecPos

showTok (TCOMM_OPEN _) = "/*"
showTok (TCOMM_CLOSE _) = "*/"
showTok (TKW s p) = s
showTok (TKID s p) = s
showTok (TKOP s p) = s
showTok (TKFILE s p) = s
showTok (TKDEF s p) = s
showTok (TKSTRING s p) = s
showTok (TKC_DEC s p) = s
showTok (TKC_EXP s p) = s
showTok (TKC_OCT s p) = s
showTok (TKC_HEX s p) = s
showTok (TKC_CHAR s p) = s 



posFromTok (TCOMM_OPEN p) = ap2sp p
posFromTok (TCOMM_CLOSE p) = ap2sp p
posFromTok (TKW s p) = ap2sp p
posFromTok (TKID s p) = ap2sp p
posFromTok (TKOP s p) = ap2sp p
posFromTok (TKFILE s p) = ap2sp p
posFromTok (TKDEF s p) = ap2sp p
posFromTok (TKC_DEC s p) = ap2sp p
posFromTok (TKC_EXP s p) = ap2sp p
posFromTok (TKC_OCT s p) = ap2sp p
posFromTok (TKC_HEX s p) = ap2sp p
posFromTok (TKC_CHAR s p) = ap2sp p
posFromTok (TKSTRING s p) = ap2sp p

ap2sp (AlexPn a b c) = newPos "" b c

-- A parser that parses a token based on a test

tokenTest x tf = token showTok posFromTok testOne where
  testOne t = if (tf x t) then Just t else Nothing

-- Commentaries

commOpen = tokenTest (TCOMM_OPEN undefined) tf where
  tf (TCOMM_OPEN _) (TCOMM_OPEN _) = True
  tf _ _ = False

commClose = tokenTest (TCOMM_CLOSE undefined) tf where
  tf (TCOMM_CLOSE _) (TCOMM_CLOSE _) = True
  tf _ _ = False

-- TKFILE

anyFile = tokenTest (TKFILE undefined undefined) tf where
  tf (TKFILE _ _) (TKFILE _ _) = True
  tf _ _ = False

-- TKSTRING

anyString = tokenTest (TKSTRING undefined undefined) tf where
  tf (TKSTRING _ _) (TKSTRING _ _) = True
  tf _ _ = False

-- TKDEF

anyDef = tokenTest (TKDEF undefined undefined) tf where
  tf (TKDEF _ _) (TKDEF _ _) = True
  tf _ _ = False

-- Constant

decConst = tokenTest (TKC_DEC undefined undefined) tf where
  tf (TKC_DEC _ _) (TKC_DEC _ _) = True
  tf _ _ = False

octConst = tokenTest (TKC_OCT undefined undefined) tf where
  tf (TKC_OCT _ _) (TKC_OCT _ _) = True
  tf _ _ = False

hexConst = tokenTest (TKC_HEX undefined undefined) tf where
  tf (TKC_HEX _ _) (TKC_HEX _ _) = True
  tf _ _ = False

expConst = tokenTest (TKC_EXP undefined undefined) tf where
  tf (TKC_EXP _ _) (TKC_EXP _ _) = True
  tf _ _ = False

charConst = tokenTest (TKC_CHAR undefined undefined) tf where
  tf (TKC_CHAR _ _) (TKC_CHAR _ _) = True
  tf _ _ = False

anyConst = choice [try decConst, try octConst, try hexConst, try expConst, try charConst]

-- Operation

tkOp s = tokenTest (TKOP s undefined) (==)

opString s = do t <- tkOp s
                return s

-- Keyword

tkKw s = tokenTest (TKW s undefined) (==)

kwString s = do t <- tkKw s
                return s
            

-- Any Keyword

anyKw = tokenTest (TKW undefined undefined) tf where
  tf (TKW _ _) (TKW _ _) = True
  tf _ _ = False

-- TKID

anyId = tokenTest (TKID undefined undefined) tf where
  tf (TKID _ _) (TKID _ _) = True
  tf _ _ = False

anyIdString = do i <- anyId
                 return (showTok i)


