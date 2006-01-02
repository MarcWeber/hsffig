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


testConst fn cnst = system cmdline
  where cmdline = "echo '#include " ++ 
                  fn ++
                  "\nstatic int a = " ++
                  cnst ++ ";' | gcc -pipe -x c -q -fsyntax-only - 2>/dev/null"

finalizeFileName Nothing = "\"@@INCLUDEFILE@@\""
finalizeFileName (Just s) = "\""++s++"\""

finalizeModuleName Nothing = "@@MODULENAME@@"
finalizeModuleName (Just s) = map d2uu (head $ reverse $ parts (== '/') s)
                                where d2uu c
                                        | c == '.' = '_'
                                        | isAlpha c = toUpper c
                                        | otherwise = c

