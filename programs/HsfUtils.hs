-- Functions common between the HSC and the XML writers.

module HsfUtils where

import C_Lexer
import C_BNF

import System.IO
import System.Cmd
import System.Exit
import Data.Char
import Data.Maybe
import Data.Either
import System.Process
import Control.Concurrent
import Text.ParserCombinators.Parsec
import qualified Data.Map as Map

-- Guessed type of a constant (via #define).

data GuessType =
  NoGuess                       -- type cannot be guessed, not a valid constant
 |Vague                         -- most likely a constant, but cannot guess type, run via gcc
 |GuessInt                      -- guessed an integer constant
 |GuessFloat                    -- guessed a floating point constant
  deriving (Ord, Eq, Show)

-- Utility: pipe processes together. Two StdStream's must be supplied: for pipeline's
-- stdin and stdout. Stderr's of all processes are as set in the process creation
-- descriptors. List of process handles is returned as well as handles for pipe ends.

runPipe :: StdStream            -- stdin
        -> StdStream            -- stdout
        -> [CreateProcess]      -- processes (start at left, end at right)
        -> IO (Maybe Handle     -- pipe in
              ,Maybe Handle     -- pipe out
              ,[ProcessHandle]) -- same order as in the input list of descriptors

runPipe ins outs [] = return (Nothing, Nothing, [])

runPipe ins outs [p] = do
  let p' = p {std_in = ins, std_out = outs}
  (pin, pout, _, ph) <- createProcess p'
  return (pin, pout, [ph])

runPipe ins outs (pstart:ps) = do
  let pstart' = pstart {std_in = ins}
      prev@(pend:rps) = reverse (pstart':ps)
      pend' = pend {std_out = outs}
  piper (pend':rps) [] Nothing Nothing

piper :: [CreateProcess] 
      -> [ProcessHandle] 
      -> Maybe Handle
      -> Maybe Handle
      -> IO (Maybe Handle, Maybe Handle, [ProcessHandle])

piper [] phs pin pend = return (pin, pend, phs)

piper (p:ps) [] Nothing Nothing = do
  let p' = p {std_in = CreatePipe}
  (pin, pend, _, ph) <- createProcess p'
  piper ps [ph] pin pend

piper (p:ps) phs pin pend = do
  let p' = p {std_in = CreatePipe, std_out = UseHandle (fromJust pin)}
  (pin', _, _, ph) <- createProcess p'
  piper ps (ph:phs) pin' pend


-- A version of words, but works with any lists on any predicate.

parts pred s = case dropWhile pred s of
                                     [] -> []
                                     s' -> w : parts pred s''
                                         where (w, s'') = break pred s'

-- Run a compiler over a stream of consts, resolving them via preprocessor,
-- try to guess their types.

guessConsts fn gcc cnsts = do
  let delimc = '%'
      onestr s = concat [[delimc], s]
      cnstrs = "#include " ++ fn ++ "\n" ++ (unlines $ map onestr cnsts)
      gccproc = CreateProcess {
                  cmdspec = RawCommand gcc ["-E", "-"]
                 ,cwd = Nothing
                 ,env = Nothing
                 ,std_in = Inherit
                 ,std_out = Inherit
                 ,std_err = Inherit
                 ,close_fds = True
                }
      grepproc = CreateProcess {
                   cmdspec = RawCommand "grep" [[delimc]]
                  ,cwd = Nothing
                  ,env = Nothing
                  ,std_in = Inherit
                  ,std_out = Inherit
                  ,std_err = Inherit
                  ,close_fds = True
                 }
  (mbin, mbout, ps) <- runPipe CreatePipe CreatePipe [gccproc, grepproc]
  case (mbin, mbout) of
    (Just i, Just h) -> do
      forkIO (hPutStrLn i cnstrs >> return ())
      s <- hGetContents h >>= return . lines
      let cprs = map (span (/= '%')) s
          tryp = zip cnsts (map (tryparse . tail . snd) cprs)
      mapM_ waitForProcess ps
      return tryp
    (_, _) -> return [("-- Error", tryparse "")]

tryparse "" = (NoGuess, [])

tryparse x = let t = scanHeader x
                 p = runParser constant_expression (PState 0 Map.empty) "" t         
 in case (t, p) of
      (_, Left _) -> (NoGuess, [])
      ([TKC_HEX _ _], Right _) -> (GuessInt, t)
      ([TKC_OCT _ _], Right _) -> (GuessInt, t)
      ([TKC_DEC _ _], Right _) -> (GuessInt, t)
      ([TKC_EXP _ _], Right _) -> (GuessFloat, t)
      (_, Right _) -> (Vague, t)

-- Run a compiler to check if a #define is good for inclusion into the
-- produced .hsc code: a #define is a simple constant, w/o arguments.

testConst fn gcc cnst = rawSystem "sh" ["-c", cmdline]
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


