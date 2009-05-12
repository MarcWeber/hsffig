-- Functions common between the HSC and the XML writers.

module HsfUtils where

import System.Cmd
import System.Exit
import Data.Char

-- A version of words, but works with any lists on any predicate.

parts pred s = case dropWhile pred s of
                                     [] -> []
                                     s' -> w : parts pred s''
                                         where (w, s'') = break pred s'

-- Run a compiler to check if a #define is good for inclusion into the
-- produced .hsc code: a #define is a simple constant, w/o arguments.

testConst fn cnst gcc = system cmdline
  where cmdline = "echo '#include " ++ 
                  fn ++
                  "\nstatic int a = " ++
                  cnst ++ ";' | " ++ gcc ++ " -pipe -x c -fsyntax-only - 2>/dev/null"

-- Based on the include file name availability, produce either the file / module names
-- themselves or placeholders for further sed'ing.

finalizeFileName Nothing = "\"@@INCLUDEFILE@@\""
finalizeFileName (Just s) = "\""++s++"\""

finalizeModuleName Nothing = "@@MODULENAME@@"
finalizeModuleName (Just s) = map d2uu (head $ reverse $ parts (== '/') s)
                                where d2uu c
                                        | c == '.' = '_'
                                        | isAlpha c = toUpper c
                                        | otherwise = c

-- Interleave a list of strings with a string.

intlv [] _ = ""
intlv [x] _ = x
intlv (rt:rts) s = rt ++ s ++ (intlv rts s)

-- Check if a structure/union name relate to anonymous structure/union

isAnon ('s':'t':'r':'u':'c':'t':'@':s:_) = isDigit s
isAnon ('u':'n':'i':'o':'n':'@':s:_) = isDigit s
isAnon _  = False


