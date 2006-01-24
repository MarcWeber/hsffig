-- A Toplevel Driver to Produce a Haskell Package out of C Header File(s)

module Main where

import Text.ParserCombinators.ReadP
import System.Console.GetOpt
import System.Environment
import System.FilePath
import System.Directory
import System.Exit
import System.IO
import System.Process
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types
import Control.Concurrent
import Control.Monad
import Data.Version
import Data.Maybe
import Data.Char
import Makefile
import HsfUtils
import HGmain
import SPmain

-- Data type to encode command line options.

data PkgArg = Verbose
            | ShowVn
            | InclFile
            | IncPath String
            | LibPath String
            | LibFile String
            | HelpMsg
            | Static
            | Make    String
            | Awk     String
            | Ar      String
            | Ghc     String
            | Gcc     String
            | Hsc2hs  String
            | PkgName String
            | PkgVn   String
            deriving (Eq, Show)

-- Control structure for getOpt.

pkgOpt :: [OptDescr PkgArg]

pkgOpt = [
  Option ['v']  ["verbose"]         (NoArg Verbose)      "provide verbose output",
  Option ['s']  ["static"]          (NoArg Static)       "prefer static libraries",
  Option ['i']  ["header"]          (NoArg InclFile)     "stop after writing package include file",
  Option ['?','h'] ["help"]         (NoArg HelpMsg)      "print this help message",
  Option ['I']  []                  (ReqArg IncPath "")  "include files location (may be multiple)",
  Option ['L']  []                  (ReqArg LibPath "")  "library files location (may be multiple)",
  Option ['l']  []                  (ReqArg LibFile "")  "library file to link (may be multiple)",
  Option ['V']  ["version"]         (NoArg ShowVn)       "show version number",
  Option ['w']  ["package-version"] (ReqArg PkgVn dfVn)  "specify version of the package",
  Option ['p']  ["package-name"]    (ReqArg PkgName "")  "name the package (will be uppercased)"
  ] ++ map pgmOpt [
    ("make", Make),
    ("awk",  Awk),
    ("ar",   Ar),
    ("ghc",  Ghc),
    ("gcc",  Gcc),
    ("hsc2hs", Hsc2hs)
    ]

pgmOpt (pgm, cons) = Option [] 
                            ["with-" ++ pgm] 
                            (ReqArg cons pgm) 
                            ("path to " ++ pgm)

parseOpt argv = 
  case getOpt Permute pkgOpt argv of
    (o,n,[]  ) -> if ((length o) + (length n) == 0) 
                    then do putStrLn $ usageInfo header pkgOpt
                            return ([],[])
                    else if HelpMsg `elem` o
                           then do putStrLn $ usageInfo header pkgOpt
                                   return ([],[])
                           else return (o,n)
    (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header pkgOpt))
  where header = "Usage: ffipkg [OPTION...] include-file..."

-- Data type for processed options and file paths.

data OptInfo = OptInfo {
  makePath :: Maybe FilePath,
  awkPath :: Maybe FilePath,
  arPath :: Maybe FilePath,
  ghcPath :: Maybe FilePath,
  gccPath :: Maybe FilePath,
  hsc2hsPath :: Maybe FilePath,
  inclDirs :: [String],
  libDirs :: [String],
  inclFiles :: [String],
  libFiles :: [String],
  pkgName :: String,
  pkgVersion :: String,
  pkgInclude :: String,
  useStatic :: Bool,
  beVerbose :: Bool,
  mkfOnly :: Bool,
  hdrOnly :: Bool
} deriving (Show)

-- Convert a version string into a Version.
-- If version is not specified, or cannot be parsed,
-- default version string (dfVn) is returned wrapped
-- in the Version type.

dfVn = "0.0" -- must be parseable!

strVersion :: String -> Version

strVersion s = let getv ls = [ r | (r,s) <- ls, s == "" ]
               in  case getv (readP_to_S parseVersion s) of
                     [] -> strVersion dfVn
                     z -> head z

-- Find and executable along the default PATH.

findExec exn = do path <- getEnv "PATH"
                  findFileAlong path exn exeExtension

-- Check executable permissions.

chkExec :: Maybe FilePath -> IO Bool

chkExec exn = do
  case exn of
    Nothing -> return False
    Just exn' -> catch (getPermissions exn' >>= return . executable) (\e -> return False)

-- Print a message when the first argument is True

infoMsg :: Bool -> String -> IO ()

infoMsg a s = when a $ putStr s

infoMsgLn :: Bool -> String -> IO ()

infoMsgLn a s = when a $ putStrLn s

-- Default option values.

defaultOptInfo :: IO OptInfo

defaultOptInfo = 
  return OptInfo `ap` findExec "make"
                 `ap` findExec "awk"
                 `ap` findExec "ar"
                 `ap` findExec "ghc"
                 `ap` findExec "gcc"
                 `ap` findExec "hsc2hs"
                 `ap` return []
                 `ap` return []
                 `ap` return []
                 `ap` return []
                 `ap` return ""
                 `ap` return dfVn
                 `ap` return ""
                 `ap` return False
                 `ap` return False
                 `ap` return False
                 `ap` return False


-- Update the OptInfo record from a parsed option
-- Note that lists of include/library directories
-- are created in reverse order: they should be supplied
-- to gcc/ghc reversed to ensure "left to right" scanning order.

updOptInfo :: OptInfo -> PkgArg -> OptInfo  

updOptInfo oi Static      = oi {useStatic = True}
updOptInfo oi Verbose     = oi {beVerbose = True}
updOptInfo oi InclFile    = oi {hdrOnly = True}
updOptInfo oi (Make s)    = oi {makePath = Just s}
updOptInfo oi (Awk s)     = oi {awkPath = Just s}
updOptInfo oi (Ar s)      = oi {arPath = Just s}
updOptInfo oi (Ghc s)     = oi {ghcPath = Just s}
updOptInfo oi (Gcc s)     = oi {gccPath = Just s}
updOptInfo oi (Hsc2hs s)  = oi {hsc2hsPath = Just s}
updOptInfo oi (IncPath s) = oi {inclDirs = s : (inclDirs oi)}
updOptInfo oi (LibPath s) = oi {libDirs = s : (libDirs oi)}
updOptInfo oi (LibFile s) = oi {libFiles = s : (libFiles oi)}
updOptInfo oi (PkgName s) = oi {pkgName = (map toUpper s), 
                                pkgInclude = "hs_" ++ (map toLower s) `joinFileExt` "h"}
updOptInfo oi (PkgVn   s) = oi {pkgVersion = showVersion $ strVersion s}
updOptInfo oi _           = oi

-- Update the default options from the options parse result.
-- All non-options are include file names.

updOptions :: ([PkgArg], [String]) -> OptInfo -> OptInfo

updOptions (op, nop) oi = (upd2 op) (oi {inclFiles = nop}) where
  upd2 [] oi = oi
  upd2 (o:os) oi = (upd2 os) (updOptInfo oi o)

-- Guess the package name from the list of header files.
-- To be applied AFTER updOptions. If PkgName (-p) was supplied,
-- fine: leave as is. Otherwise use the name of the first include
-- file with two underscores prepended for the package include
-- file, and the same name uppercased with dots replaced with
-- underscores as package name. Don't forget to strip directory
-- from the file name.

guessPkgName :: OptInfo -> OptInfo

guessPkgName oi@(OptInfo {inclFiles = []}) = oi
guessPkgName oi = oi {pkgName = pkg, pkgInclude = "hs_" ++ incl1} where
  incl1 = if pkgName oi /= "" 
    then pkgInclude oi
    else (snd . splitFileName . head . inclFiles) oi
  pkg = if pkgName oi /= ""
    then pkgName oi
    else finalizeModuleName (Just $ (fst . splitFileExt) incl1)
  

-- Borrowed from Cabal: find a file along search path.
-- The path must be a properly separated string of directories.

findFileAlong :: String -> String -> String -> IO (Maybe FilePath)

findFileAlong path file ext = do
  search (parseSearchPath path)
  where
    search :: [FilePath] -> IO (Maybe FilePath)
    search [] = return Nothing
    search (d:ds) = do
       let path = d `joinFileName` file `joinFileExt` ext
       b <- doesFileExist path
       if b then return (Just path)
             else search ds

-- Check executablilty verbosely.

chkExecVerb :: Bool -> (String, Maybe FilePath) -> IO (String, Bool)

chkExecVerb b (s, mfp) =
  case mfp of
    Nothing -> do infoMsgLn b $ 
                    "Path for " ++ s ++ " was not supplied nor was it found along the PATH"
                  return (s, False)
    Just fp -> do infoMsg b $ s ++ " ... "
                  e <- chkExec mfp
                  infoMsg b $ fp ++ ": "
                  infoMsgLn b $ if e then "OK" else "Failed"
                  return (s, e)

-- Find an executable program verbosely.

findExecVerb :: Bool -> String -> IO (String, Maybe FilePath)

findExecVerb b s = do
  infoMsg b $ s ++ " ... "
  mfp <- findExec s
  e <- chkExec mfp
  if e
    then do infoMsgLn b $ fromJust mfp
            return (s, mfp)
    else do infoMsgLn b "Not found/ not executable: check your PATH"
            return (s, Nothing)

-- Create a file and open a Fd on it.

fileToFd :: FilePath -> IO Fd

fileToFd s = openFd s WriteOnly (Just 420) defaultFileFlags

-- Redirect a Fd in a forked process.
-- Credits to Donn Cave for the tip how to redirect stdout.

redirFd :: Fd -> Fd -> IO a -> IO a

redirFd new old fn = do
  when (old >= 0) $ dupTo new old >> return ()
  closeFd new
  fn

-- List of extra programs needed by Makefile: they are expected
-- to be on the PATH. If any of them is not on the PATH,
-- the program aborts.

xProgs = ["echo", "rm", "find", "grep", "mkdir", "touch", "true"]

-- The Main Program.

main = do
  opts <- getArgs >>= parseOpt
  dopt <- defaultOptInfo >>= (return . (guessPkgName . updOptions opts))
  putStrLn $ show opts
  putStrLn $ show dopt
  let minusI = map ("-I" ++) (reverse $ inclDirs dopt)
      fileBase = "HS_" ++ pkgName dopt ++ "_H"
      hscFile = fileBase `joinFileExt` "hsc"
      hsuFile = fileBase `joinFileExt` "hs_unsplit"
      libFile = "lib" ++ (fileBase `joinFileExt` "a")
      cabalFile = (pkgName dopt) `joinFileExt` "cabal"
  infoMsgLn (beVerbose dopt) "Checking existence of the programs supplied..."
  excs <- mapM (chkExecVerb $ beVerbose dopt) [("make",    makePath dopt), 
                                               ("awk",     awkPath dopt),
                                               ("ar",      arPath dopt),
                                               ("gcc",     gccPath dopt),
                                               ("ghc",     ghcPath dopt),
                                               ("hsc2hs",  hsc2hsPath dopt)]
  infoMsgLn (beVerbose dopt) "Checking existence of the programs needed by Makefile..."
  progs <- mapM (findExecVerb $ beVerbose dopt) xProgs
  let exfail = [s | (s, b) <- excs, not b] ++ [s | (s, m) <- progs, m == Nothing]
  when ((length exfail) > 0) $ do
    putStrLn $ "Failed: The following programs cannot execute:"
    mapM putStrLn exfail 
    exitWith (ExitFailure 1)
  infoMsgLn (beVerbose dopt) "Creating the package header file..."
  h <- openFile (pkgInclude dopt) WriteMode
  hPutStrLn h "/* File is generated automatically: do not edit */"
  mapM (\s -> hPutStrLn h $ "#include \"" ++ s ++ "\"") (inclFiles dopt)
  hClose h
  when (hdrOnly dopt) $ exitWith (ExitSuccess)
  infoMsgLn (beVerbose dopt) "Running gcc and producing the hsc file..."
  (fd1, fd2) <- createPipe
  hscfd <- fileToFd hscFile
  hscpid <- forkProcess $ redirFd fd1 0 $
                          redirFd fd2 (-1) $
                          redirFd hscfd 1 $
                          hsffigMain (fromJust $ gccPath dopt)
                                     (inclDirs dopt)
  gccpid <- forkProcess $ redirFd fd2 1 $
                          executeFile (fromJust $ gccPath dopt)
                                      False
                                      (["-E", "-dD"] ++
                                       minusI ++
                                       [pkgInclude dopt])
                                      Nothing
  closeFd hscfd
  closeFd fd1
  closeFd fd2
  gccrt <- getProcessStatus True False gccpid
  hscrt <- getProcessStatus True False hscpid
  let hscfail = map fst $ 
                    filter (\(d, r) -> r /= Just (Exited ExitSuccess)) [
                      ("of gcc", gccrt),
                      ("while producing the hsc file", hscrt)]
  when ((length hscfail) > 0) $ do
    mapM (\s -> putStrLn $ "Failed: abnormal termination " ++ s) hscfail
    exitWith (ExitFailure 2)
  infoMsgLn (beVerbose dopt) "Running hsc2hs..."
  h2hpid <- forkProcess $ executeFile (fromJust $ hsc2hsPath dopt)
                                      False
                                      (["-t", "/dev/null", hscFile, 
                                       "-o", hsuFile] ++
                                         minusI)
                                      Nothing
  h2hrt <- getProcessStatus True False h2hpid
  when (h2hrt /= Just (Exited ExitSuccess)) $ do
    putStrLn "Failed: abnormal termination of hsc2hs"
    exitWith (ExitFailure 3)
  infoMsgLn (beVerbose dopt) "Running splitter..."
  modlist <- splitterMain [hsuFile]
  when ((length modlist) == 0) $ do
    putStrLn "Failed: splitter yielded empty list of modules"
    exitWith (ExitFailure 4)
  infoMsgLn (beVerbose dopt) $ "Splitter yielded " ++ show (length modlist) ++ " modules"
  infoMsgLn (beVerbose dopt) "Creating Makefile..."
  mkffd <- fileToFd "Makefile"
  mkfpid <- forkProcess $ redirFd mkffd 1 $ do
    putStrLn $ "# Makefile is generated automatically: do not edit"
    putStrLn $ "# This Makefile builds a library for the package " ++ pkgName dopt
    putStrLn $ ""
    putStrLn $ "AR = " ++ (fromJust $ arPath dopt)
    putStrLn $ "AWK = " ++ (fromJust $ awkPath dopt)
    putStrLn $ "MAKE = " ++ (fromJust $ makePath dopt)
    putStrLn $ "GCC = " ++ (fromJust $ gccPath dopt) ++ " " ++ intlv minusI " "
    putStrLn $ "GHC = " ++ (fromJust $ ghcPath dopt) ++ " " ++ intlv minusI " "
    mapM (\(s, m) -> putStrLn $ (map toUpper s) ++ " = " ++ fromJust m) progs
    putStrLn $ ""
    putStrLn $ "all: " ++ libFile
    putStrLn $ ""
    writeMakefile
    return ()
  closeFd mkffd
  mkfrt <- getProcessStatus True False mkfpid
  when (mkfrt /= Just (Exited ExitSuccess)) $ do
    putStrLn "Failed: abnormal termination while writing Makefile"
    exitWith (ExitFailure 5)
  infoMsgLn (beVerbose dopt) $ "Creating " ++ cabalFile ++ "..."
  cabfd <- fileToFd cabalFile
  cabpid <- forkProcess $ redirFd cabfd 1 $ do
    putStrLn $ "-- " ++ cabalFile ++ " is generated automatically: do not edit"
    putStrLn $ "Name: " ++ pkgName dopt
    putStrLn $ "Version: " ++ pkgVersion dopt
    putStrLn $ "Build-depends: HSFFIG"
    putStrLn $ "Exposed-modules: " ++ head modlist
    putStrLn $ "Other-modules: " ++ intlv (drop 1 modlist) ", "
    return ()
  closeFd cabfd
  cabrt <- getProcessStatus True False cabpid
  when (cabrt /= Just (Exited ExitSuccess)) $ do
    putStrLn $ "Failed: abnormal termination while writing " ++ cabalFile
    exitWith (ExitFailure 6)
  infoMsgLn (beVerbose dopt) "Creating Setup.hs"
  setfd <- fileToFd "Setup.hs"
  setpid <- forkProcess $ redirFd setfd 1 $ do
    putStrLn $ "-- Setup.hs is generated automatically: do not edit"
    putStrLn $ "module Main (main) where"
    putStrLn $ "import Distribution.Simple (defaultMainWithHooks, defaultUserHooks)"
    putStrLn $ "main = defaultMainWithHooks defaultUserHooks"
    return ()
  closeFd setfd
  setrt <- getProcessStatus True False setpid
  when (setrt /= Just (Exited ExitSuccess)) $ do
    putStrLn $ "Failed: abnormal termination while writing Setup.hs"
    exitWith (ExitFailure 7)
  exitWith ExitSuccess
