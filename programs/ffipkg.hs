-- A Toplevel Driver to Produce a Haskell Package out of C Header File(s)

module Main where

import Text.ParserCombinators.ReadP
import Distribution.Simple.BuildPaths
import Distribution.Simple.Configure
import Distribution.Compiler
import qualified Distribution.Package as DP
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
import Setupfile
import SetupfileNew
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
            | CppOpt  String
            | HelpMsg
            | NewHooks
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
  Option ['n']  ["new-hooks"]       (NoArg NewHooks)     "use newer userHooks interface",
  Option ['i']  ["header"]          (NoArg InclFile)     "stop after writing package include file",
  Option ['?','h'] ["help"]         (NoArg HelpMsg)      "print this help message",
  Option ['I']  []                  (ReqArg IncPath "")  "include files location (may be multiple)",
  Option ['L']  []                  (ReqArg LibPath "")  "library files location (may be multiple)",
  Option ['l']  []                  (ReqArg LibFile "")  "library file to link (may be multiple)",
  Option ['c']  ["cpp"]             (ReqArg CppOpt  "")  "option for CPP (may be multiple)",
  Option ['V']  ["version"]         (NoArg ShowVn)       "show program version number",
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

-- A function to describe a "path-to-a-program" option uniformly.

pgmOpt (pgm, cons) = Option [] 
                            ["with-" ++ pgm] 
                            (ReqArg cons pgm) 
                            ("path to " ++ pgm)

-- Parse the command line options. If nothing on the command line
-- then just print the help message.

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
  cppOpts :: [String],
  pkgName :: String,
  pkgVersion :: String,
  useNewHooks :: Bool,
  beVerbose :: Bool,
  mkfOnly :: Bool,
  hdrOnly :: Bool,
  showVn :: Bool
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
                 `ap` return []
                 `ap` return ""
                 `ap` return dfVn
                 `ap` return False
                 `ap` return False
                 `ap` return False
                 `ap` return False
                 `ap` return False


-- Update the OptInfo record from a parsed option
-- Note that lists of include/library directories
-- are created in reverse order: they should be supplied
-- to gcc/ghc reversed to ensure "left to right" scanning order.

updOptInfo :: OptInfo -> PkgArg -> OptInfo  

updOptInfo oi NewHooks    = oi {useNewHooks = True}
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
updOptInfo oi (CppOpt  s) = oi {cppOpts  = s : (cppOpts  oi)}
updOptInfo oi (PkgName s) = oi {pkgName = (map toUpper s)} 
updOptInfo oi (PkgVn   s) = oi {pkgVersion = showVersion $ strVersion s}
updOptInfo oi ShowVn      = oi {showVn = True}
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
guessPkgName oi = oi {pkgName = pkg} where
  pkg = if pkgName oi /= ""
    then pkgName oi
    else finalizeModuleName $ Just $ 
                              (fst . splitExtensions) $ 
                              (snd . splitFileName . head . inclFiles) oi
  

-- Borrowed from Cabal: find a file along search path.
-- The path must be a properly separated string of directories.

findFileAlong :: String -> String -> String -> IO (Maybe FilePath)

findFileAlong path file ext = do
  search (splitSearchPath path)
  where
    search :: [FilePath] -> IO (Maybe FilePath)
    search [] = return Nothing
    search (d:ds) = do
       let path = d </> file <.> ext
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

xProgs = ["echo", "rm", "find", "grep", "mkdir", "touch", "true", "cp", "mv", "ld"]

-- The Main Program.

main = do

-- Obtain command line parameters. If -V (show version) is specified,
-- then query Cabal for all packages installed, find ourselves,
-- retrieve the version number.

  opts <- getArgs >>= parseOpt
  dopt <- defaultOptInfo >>= (return . (guessPkgName . updOptions opts))
  when (showVn dopt) $ do
    let nverb = if (beVerbose dopt) then 5 else 0
    comp <- configCompiler (Just GHC) (ghcPath dopt) Nothing 0
    pkgs <- getInstalledPackages comp False nverb
    let thispkg =
          filter (\p -> map toUpper (DP.pkgName (p :: DP.PackageIdentifier)) == "HSFFIG") pkgs
    if (length thispkg == 0)
      then do
        infoMsgLn True "Package HSFFIG is not installed: cannot determine my version"
        exitWith (ExitFailure 9)
      else do
        pgm <- getProgName
        infoMsgLn True $ pgm ++ " version " ++ (showVersion $ DP.pkgVersion $ head thispkg)
        exitWith ExitSuccess

-- If there are no header files (non-option parameters) specified, terminate.

  case opts of
    ([], []) -> exitWith ExitSuccess
    (_, []) ->  do infoMsgLn True "No header files provided"
                   exitWith (ExitFailure 10)
    other -> do return ()

-- Define some frequently used functions.

-- All -I options (include directories) from the command line. 
-- Reverse is necessary because options were taken from 
-- the command line and included into a list in inverse order.

  let minusI = map ("-I" ++) (reverse $ inclDirs dopt)

-- All -L options (librray directories). Similarly, requires reverse.

      minusL = map ("-L" ++) (reverse $ libDirs dopt)

-- All other options to be passed to the C preprocessor/compiler.
-- May be anything, but most frequently will be used for "-DX=Y"
-- constructs.

      minusD = reverse $ cppOpts dopt

-- Name of the include file to be converted to hsc.

      incFile = ("hs_" ++ map toLower (pkgName dopt)) <.> "h"

-- Base part of all package-dependent file names.

      fileBase = "HS_" ++ pkgName dopt ++ "_H"

-- Name of the hsc file after hsffig.

      hscFile = fileBase <.> "hsc"

-- Name of the Haskell file after hsc2hs.

      hsuFile = fileBase <.> "hs_unsplit"

-- Name of the package library file.

      libFile = "lib" ++ (fileBase <.> "a")

-- Name of the Cabal package desctiption file generated at the end.

      cabalFile = (pkgName dopt) <.> "cabal"

-- Check whether the programs supplied on the command line (if any)
-- exist and have executable attribute.

  infoMsgLn (beVerbose dopt) "Checking existence of the programs supplied..."
  excs <- mapM (chkExecVerb $ beVerbose dopt) [("make",    makePath dopt), 
                                               ("awk",     awkPath dopt),
                                               ("ar",      arPath dopt),
                                               ("gcc",     gccPath dopt),
                                               ("ghc",     ghcPath dopt),
                                               ("hsc2hs",  hsc2hsPath dopt)]

-- Same for the "extra" programs needed by Makefile.

  infoMsgLn (beVerbose dopt) "Checking existence of the programs needed by Makefile..."
  progs <- mapM (findExecVerb $ beVerbose dopt) xProgs

-- See which programs were not found/were not executable. Print the list of programs.

  let exfail = [s | (s, b) <- excs, not b] ++ [s | (s, m) <- progs, m == Nothing]
  when ((length exfail) > 0) $ do
    putStrLn $ "Failed: The following programs cannot execute:"
    mapM putStrLn exfail 
    exitWith (ExitFailure 1)

-- Place names of all the include files specified on the command line
-- into the package include file.

  infoMsgLn (beVerbose dopt) "Creating the package header file..."
  h <- openFile incFile WriteMode
  hPutStrLn h "/* File is generated automatically: do not edit */"
  mapM (\s -> hPutStrLn h $ "#include \"" ++ s ++ "\"") (inclFiles dopt)
  hClose h
  when (hdrOnly dopt) $ exitWith (ExitSuccess)

-- Run the pipeline consisting of the C preprocessor and the hsffig
-- main program. Pass all preprocessor options (including include directories)
-- to the preprocessor.

  infoMsgLn (beVerbose dopt) "Running gcc and producing the hsc file..."
  (fd1, fd2) <- createPipe
  hscfd <- fileToFd hscFile
  hscpid <- forkProcess $ redirFd fd1 0 $
                          redirFd fd2 (-1) $
                          redirFd hscfd 1 $
                          hsffigMain (fromJust $ gccPath dopt)
                                     (inclDirs dopt) minusD
  gccpid <- forkProcess $ redirFd fd2 1 $
                          executeFile (fromJust $ gccPath dopt)
                                      False
                                      (["-E", "-dD"] ++
                                       minusI ++ minusD ++
                                       [incFile])
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

-- Run hsc2hs.

  infoMsgLn (beVerbose dopt) "Running hsc2hs..."
  h2hpid <- forkProcess $ executeFile (fromJust $ hsc2hsPath dopt)
                                      False
                                      (["-t", "/dev/null", hscFile, 
                                       "-o", hsuFile] ++
                                         minusI ++ minusD)
                                      Nothing
  h2hrt <- getProcessStatus True False h2hpid
  when (h2hrt /= Just (Exited ExitSuccess)) $ do
    putStrLn "Failed: abnormal termination of hsc2hs"
    exitWith (ExitFailure 3)

-- This utility always splits source files as the size of the hsc file
-- is impossible to predict.

  infoMsgLn (beVerbose dopt) "Running splitter..."
  modlist <- splitterMain [hsuFile]
  when ((length modlist) == 0) $ do
    putStrLn "Failed: splitter yielded empty list of modules"
    exitWith (ExitFailure 4)
  infoMsgLn (beVerbose dopt) $ "Splitter yielded " ++ show (length modlist) ++ " modules"

-- Create Makefile. Most part of it is in the Makefile template, so here
-- only paths to the programs needed by Makefile are hardcoded in the Makefile.

  infoMsgLn (beVerbose dopt) "Creating Makefile..."
  mkffd <- fileToFd "Makefile"
  mkfpid <- forkProcess $ redirFd mkffd 1 $ do
    putStrLn $ "# Makefile is generated automatically: do not edit"
    putStrLn $ "# This Makefile builds a library for the package " ++ pkgName dopt
    putStrLn $ ""
    putStrLn $ "AR = " ++ (fromJust $ arPath dopt)
    putStrLn $ "AWK = " ++ (fromJust $ awkPath dopt)
    putStrLn $ "MAKE = " ++ (fromJust $ makePath dopt)
    putStrLn $ "GCC = " ++ (fromJust $ gccPath dopt) ++ " " ++ intlv minusI " " ++ 
               intlv minusD " "
    putStrLn $ "GHC = " ++ (fromJust $ ghcPath dopt) ++ " " ++ intlv minusI " "
    mapM (\(s, m) -> putStrLn $ (map toUpper s) ++ " = " ++ fromJust m) progs
    putStrLn $ ""
    writeMakefile
    return ()
  closeFd mkffd
  mkfrt <- getProcessStatus True False mkfpid
  when (mkfrt /= Just (Exited ExitSuccess)) $ do
    putStrLn "Failed: abnormal termination while writing Makefile"
    exitWith (ExitFailure 5)

-- Create the Cabal package description file. A really minimal subset of fields
-- is needed here. List of exposed and hidden modules is obtained from the splitter.

  infoMsgLn (beVerbose dopt) $ "Creating " ++ cabalFile ++ "..."
  cabfd <- fileToFd cabalFile
  cabpid <- forkProcess $ redirFd cabfd 1 $ do
    putStrLn $ "-- " ++ cabalFile ++ " is generated automatically: do not edit"
    putStrLn $ "Name: " ++ pkgName dopt
    putStrLn $ "Version: " ++ pkgVersion dopt
    putStrLn $ "Build-depends: base, HSFFIG"
    putStrLn $ "Exposed-modules: " ++ head modlist
    putStrLn $ "Other-modules:\n" ++ intlv (map ("  " ++) (drop 1 modlist)) ",\n"
    when (length (libDirs dopt) > 0) $ 
      putStrLn $ "Extra-lib-dirs:\n" ++ intlv (map ("  " ++) (reverse (libDirs dopt))) ",\n"
    when (length (libFiles dopt) > 0) $
      putStrLn $ "Extra-libraries:\n" ++ intlv (map ("  " ++) (reverse ( libFiles dopt))) ",\n"
    return ()
  closeFd cabfd
  cabrt <- getProcessStatus True False cabpid
  when (cabrt /= Just (Exited ExitSuccess)) $ do
    putStrLn $ "Failed: abnormal termination while writing " ++ cabalFile
    exitWith (ExitFailure 6)

-- Create the Setup.hs file. It is mostly taken from the template. Choice
-- of the template is based on the -n command line option (whether newer
-- or older userHooks interface is used).

  infoMsgLn (beVerbose dopt) "Creating Setup.hs"
  setfd <- fileToFd "Setup.hs"
  setpid <- forkProcess $ redirFd setfd 1 $ do
    putStrLn $ "-- Setup.hs is generated automatically: do not edit"
    if (useNewHooks dopt) then writeSetupfileNew else writeSetupfile
    return ()
  closeFd setfd
  setrt <- getProcessStatus True False setpid
  when (setrt /= Just (Exited ExitSuccess)) $ do
    putStrLn $ "Failed: abnormal termination while writing Setup.hs"
    exitWith (ExitFailure 7)

-- Finally, the utility is done. Next, the user runs `runghc Setup.hs'
-- as usual.

  exitWith ExitSuccess

