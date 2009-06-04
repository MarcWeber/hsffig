-- Output contents of a .hsc file to standard output

module WriteHsc where

import Data.Char
import C_BNF
import SplitBounds
import Template
import Text.ParserCombinators.Parsec
import HsfUtils
import System.Exit
import qualified Data.Map as Map
import Data.Maybe
import Data.List
import Control.Monad
import System.IO

-- Name for the HSFFIG field access class, and module to import

fldclass = "HSFFIG.FieldAccess.FieldAccess"
fldmodule = "HSFFIG.FieldAccess"

-------------------------------------------------------------------------------------------

-- Write the beginning of the hsc code if the header filename
-- is known (Just ...). The beginning consists of
--  * an include statement (with header file name)
--  * module declaration (module name derived form the header file name)
--  * import statements for Foreign modules.

-- If header file name cannot be obtained (gcc -E -P) (mfn == Nothing)
-- then placeholders will be placed instead of header file name and module
-- name declared. This may be useful if something other is desired for
-- module name rather than header filename derivative.
-- Derivation of module name includes stripping file path, uppercasing the header filename
-- and replacing dots with underscores (so most likely module name will end
-- with _H).

ghcopts = "{-# OPTIONS -fglasgow-exts -XForeignFunctionInterface #-}"

writeModHdr mfn = do let fmfn = finalizeModuleName mfn
                     writeTemplate stdout
                     putStrLn $ "#def void _dummy_force_" ++ fmfn ++ "_hsc_c (void) { }"
                     putStrLn $ ghcopts
                     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfn ++ "\n"
                     putStrLn $ ghcopts
                     putStrLn $ "#include " ++ (finalizeFileName mfn)
                     putStrLn $ "#ifndef __quote__"
                     putStrLn $ "#define __quote__(x...) x"
                     putStrLn $ "#endif"
                     putStrLn $ ""
                     putStrLn $ "module " ++ fmfn ++ "("
                     putStrLn $ "  module " ++ fmfn ++ ","
                     putStrLn $ splitOpen
                     putStrLn $ "  module " ++ fmfn ++ "_C,"
                     putStrLn $ "  module " ++ fmfn ++ "_S,"
                     putStrLn $ "  module " ++ fmfn ++ "_F,"
                     putStrLn $ "  module " ++ fmfn ++ "_E,"
                     putStrLn $ "  module " ++ fmfn ++ "_S_d,"
                     putStrLn $ "  module " ++ fmfn ++ "_S_t,"
                     putStrLn $ "  module " ++ fmfn ++ "_S_n,"
                     putStrLn $ splitClose
                     putStrLn $ "  module " ++ fldmodule ++ ","
                     putStrLn $ "  module Foreign,"
                     putStrLn $ "  module Foreign.C.String,"
                     putStrLn $ "  module Foreign.C.Types) where"
                     putStrLn $ ""
                     putStrLn $ "import Foreign"
                     putStrLn $ "import Foreign.Ptr"
                     putStrLn $ "import Foreign.C.Types"
                     putStrLn $ "import Foreign.C.String"
                     putStrLn $ "import " ++ fldmodule
                     putStrLn $ splitOpen
                     putStrLn $ "import " ++ fmfn ++ "_C"
                     putStrLn $ "import " ++ fmfn ++ "_S"
                     putStrLn $ "import " ++ fmfn ++ "_F"
                     putStrLn $ "import " ++ fmfn ++ "_E"
                     putStrLn $ "import " ++ fmfn ++ "_S_d"
                     putStrLn $ "import " ++ fmfn ++ "_S_t"
                     putStrLn $ "import " ++ fmfn ++ "_S_n"
                     putStrLn $ "import " ++ fldmodule
                     putStrLn $ splitClose
                     putStrLn $ "\n" ++ splitEnd ++ "\n"

writeSplitHeaderX imps rexps mn = do
  putStrLn $ splitOpen
  putStrLn $ ghcopts
  putStrLn $ "module " ++ mn ++ " ("
  mapM (putStrLn . (\s -> "  module " ++ s ++ ",")) rexps
  putStrLn $ "  module " ++ mn
  putStrLn $ ") where\n"
  putStrLn $ "import Foreign"
  putStrLn $ "import Foreign.C.Types"
  mapM (putStrLn . ("import " ++)) imps
  putStrLn $ splitClose
  putStrLn $ ""

writeSplitHeader imps mn = writeSplitHeaderX imps [] mn

--------------------------------------------------------------------------------------

-- For every constant defined in the headers, produce accessor functions
-- using the following pattern:
-- c_CONST = #const CONST i. e. first character of the constant name
-- is lowercased.
-- Not all constants defined in the header file are good for inclusion
-- in the generated bindings code. For each, a short two-liner program
-- will be compiled. If there are no syntax errors, the constant qualifies.
-- This requires knowledge of the header file name. Therefore if it was impossible
-- to determine it, constants will not be included.

writeConstAccess tus gcc Nothing = return ()
writeConstAccess tus gcc (Just fn) = 
  do let cnsts = Map.keys $ Map.filterWithKey constonly tus
         constonly _ DictDef = True
         constonly _ _ = False
         fmfnc = (finalizeModuleName (Just fn)) ++ "_C"
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfnc ++ "\n"
     writeSplitHeader [] fmfnc
     mapM (oneconst fn gcc) cnsts 
     putStrLn $ "\n" ++ splitEnd ++ "\n"
     return ()

-- No import for #define alloca.

oneconst _ _ "alloca" = return ()

oneconst fn gcc cnst = 
  do rc <- testConst (finalizeFileName (Just fn)) cnst gcc
     case rc of
          ExitSuccess -> putStrLn $ "c_" ++ cnst ++ " = #const " ++ cnst
          _ -> return ()

---------------------------------------------------------------------------------------

-- For every enumeration, compute values for implicitly valued variants.
-- Then emit all variants as constants similarly to constants themselves.

writeEnums tus tymap fn =
  do let enums = Map.toList $ Map.filterWithKey enumsonly tymap
         enumsonly _ (DictEnum _) = True
         enumsonly _ _ = False
         enumsof (DictEnum e) = map simplifyenum e
         simplifyenum (Enumerator s Nothing) = (s,"")
         simplifyenum (Enumerator s (Just e)) = (s,show e)
         allvariants = concat $ map (fillenum "0") $ map (enumsof . snd) enums
         fmfne = (finalizeModuleName fn) ++ "_E"
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfne ++ "\n"
     writeSplitHeader [] fmfne
     mapM onevariant allvariants
     putStrLn $ "\n" ++ splitEnd ++ "\n"


onevariant (s,e) = 
  putStrLn $ "e_" ++ s ++ " = #const " ++ e

-- Fill an enumeration with explicit variants. Every implicit variant
-- is considered to be its predecessor plus 1. The first implicit variant
-- is 0.

fillenum _ [] = []
fillenum val (ed:eds) = fv : (fillenum (nxtval fv) eds) where
  fv = fixval val ed
  fixval val (s,"") = (s,val)
  fixval _ (s,e) = (s,e)
  nxtval (_,vals) = "(" ++ vals ++ ") + 1"

---------------------------------------------------------------------------------------

-- For every C structure or union, declare a newtype, and field access functions.
-- The type map will be scanned for recorded structures.

-- To access structure fields, special data constructor will be created,
-- containing variants for all field names found in the header.

-- To provide convinient syntax of field access, a class is defined in each
-- module created from a header, e. g. for `example.h':

-- class EXAMPLE_H_fieldaccess a b c | a c -> b where
--   (-->) :: Ptr a -> c -> IO b
--   (<--) :: (Ptr a, c) -> b -> IO ()

-- and for each named field an algebraic data type whose name is derived from
-- the field name:

--  data V_a = V_a 
--  data V_b = V_b
--  data V_c = V_c

-- also similar constructs with X_ and D_ prefixes.

-- The combinator, (-->), is defined to access fields of structures by their
-- derivative names. For example, given a struct STR {int blah;};, and
-- a pointer `ptrstr' returned from some function, the field blah may be
-- accessed as `ptrstr --> V_blah.

-- For the size of the structure, a fictive field, V_sizeof, is defined.

-- Functional dependencies in the class are necessary to eliminate the need
-- to explicitly specify retrieved value's type in the calling function. That is,
-- structure type (a) and field selector type (c) uniquely determine the return type (b).

-- Instance (minimal) of Storable is also provided for each structure
-- to be able to use alloca/malloc when necessary.

data StructInfo = StructInfo {
  strName :: String,				-- structure type name as supplied
  trueName :: String,				-- true name of the structure
  convName :: String,				-- converted name of the structure
  cSyntax :: String				-- C syntax layout of the structure
} deriving (Show)

data FieldInfo = FieldInfo {
  fldName :: String,				-- field name
  fldType :: String,				-- field type
  instType :: String,				-- field type for instance FieldAccess
  fldTypeString :: TypeString,			-- field type string after the TCM state machine
  fldArity :: Int,				-- field arity (0 for non-functions)
  fldDims :: String,				-- dimensions if an array ([] for scalars)
  isDynamic :: Bool,				-- field represents a dynamic import (FunPtr)
  isDirect :: Bool,				-- field represents a direct structure/union
  isVariadic :: Bool,				-- field represents a variadic function
  isBitField :: Bool,				-- bit field
  internId :: String				-- field symbol internal ID
} deriving (Show)

sd2fld (StructDeclarator (Just d) _) = id2name (InitDeclarator d Nothing)
sd2fld _ = ""

collectfields dss = concat $ map collectfields' dss where
  collectfields' (DictStruct su ss) = concat $ map collectfields'' ss where
    collectfields'' (StructDecl _ sds) = map sd2fld sds where

-- Anonymous identifiers (starting with "_@_") will not be processed.

writefields flds fn =
  mapM fldata flds where
    fldata ('_':'@':'_':_) = return ()
    fldata fld = do putStrLn $ "data V_" ++ fld ++ " = V_" ++ fld
                    putStrLn $ "data X_" ++ fld ++ " = X_" ++ fld
                    putStrLn $ "data D_" ++ fld ++ " = D_" ++ fld

writeStructures tus tymap fn = 
  do let structs = Map.filterWithKey structonly tymap
         typedefs = Map.toList $ Map.filterWithKey tdefonly tymap
         typeeqs = Map.toList $ Map.filterWithKey tteqonly tymap
         structonly _ (DictStruct _ _) = True
         structonly _ _ = False
         tdefonly _ (DictDecl _) = True
         tdefonly _ _ = False
         tteqonly _ (DictTypeEq _) = True
         tteqonly _ _ = False
         allfields = nub $ collectfields (Map.elems structs)
         fmfns = (finalizeModuleName fn) ++ "_S"
         strpairs = Map.toList structs
         strnames = map fst strpairs
         submods = map (((fmfns ++ "_") ++) . convname) strnames
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfns ++ "\n"
     writeSplitHeaderX submods submods fmfns
     putStrLn $ "\n" ++ splitEnd ++ "\n"
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfns ++ "_d\n"
     writeSplitHeader [] $ fmfns ++ "_d"
     writefields ("sizeof" : allfields) fn
     putStrLn $ "\n" ++ splitEnd ++ "\n"
     putStrLn ""
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfns ++ "_t\n"
     writeSplitHeader [fmfns ++ "_n"] $ fmfns ++ "_t"
     mapM (tdefalias tymap) (typedefs ++ typeeqs)
     putStrLn $ "\n" ++ splitEnd ++ "\n"
     putStrLn ""
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfns ++ "_n\n"
     writeSplitHeader [] $ fmfns ++ "_n"
     mapM structnewtype strnames
     putStrLn $ "\n" ++ splitEnd ++ "\n"
     mapM (onestruct tymap fn) strpairs
     return ()

-- Fill out a strinfo data structure.

mkStrInfo strname strdecl =
  StructInfo {
    strName = strname,
    trueName = if (isAnon strname) then (show strdecl) else (truename strname),
    convName = convname strname,
    cSyntax = show strdecl
  }

-- Fill out a fldinfo data structure.

mkFldInfo tymap xd = 
  let state = (dcl2ts . (connectta tymap) . simplifystructdecl) xd
      sdcl2sd (StructDecl _ s) = head s
      fldn = (sd2fld . sdcl2sd) xd
      stt = state2ts state
      cmap = mapc2hs stt
      stsp = condmonadify cmap
      condmonadify t = if (dyn t) then (monadify "IO" t) else t
      dyn t = case t of
        (PtrF _ _) -> True
        other -> False
      dir = isdirstruct cmap
      sdecl = ts2ts stsp
      bit = '|' `elem` sdecl
      instt =
        (case isarray stt of
           True -> "Ptr ("
           False -> "(" )
          ++ (case dyn stt of
                True  -> unfptr sdecl
                False -> case dir of
                  True  -> "Ptr " ++ (drop 1 sdecl)
                  False -> case bit of
                    True -> drop 1 $ dropWhile (/= '|') sdecl
                    False -> sdecl) ++ ")"
      dims = case isarray stt of
        True -> arrdims stt
        False -> []
      arity tsp = case tsp of
        TApply ts -> (length ts) - 1
        PtrF _ t -> arity t
        other -> 0
      intern = case (Map.lookup ("ID " ++ fldn) tymap) of
        Just (DictId n) -> "___" ++ (show n) ++ "___"
        other -> ""
      isarray tt = case tt of
        TString ts -> isarrt ts
        TString' ts -> isarrt ts
        PtrV _ t -> isarray t
        other -> False
      arrdims tt = case tt of
        TString ts -> (fst . splitarrt) ts
        TString' ts -> (fst . splitarrt) ts
        PtrV _ t -> arrdims t
        other -> []
  in  FieldInfo {
        fldName = fldn,
        fldType = sdecl,
        instType = instt,
        fldTypeString = stsp,
        fldArity = arity stsp,
        fldDims = dims,
        isDynamic = dyn stt,
        isDirect = dir,
        isBitField = bit,
        isVariadic = isvariadic cmap,
        internId = intern
      }

-- Toplevel wrapper to fill out structure and field descriptors.

mkStructsInfo tymap strname strdecl = 
  let xdecls = decls strdecl
      decls (DictStruct su s) = expdecls s
      strinfo = mkStrInfo strname strdecl
      fldinfos = map (mkFldInfo tymap) xdecls
  in  (strinfo, fldinfos)

-- Write complete definition for a single structure/union.

onestruct tymap fn (strname,strdecl) = 
  do let strm = fmfns ++ "_" ++ (convname strname)
         fmfns = (finalizeModuleName fn) ++ "_S"
         (strinfo, fldinfos) = mkStructsInfo tymap strname strdecl
     putStrLn ""
     putStrLn "--"
     putStrLn ""
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ strm ++ "\n"
     writeSplitHeader (fldmodule : (map (fmfns ++) ["_t", "_n", "_d"])) strm
     mapM (structinstance fn strinfo) fldinfos
     when ((length fldinfos) /= 0) $ structinstance fn strinfo FieldInfo {
       fldName = "sizeof",
       fldType = "CInt",
       instType = "CInt",
       fldArity = 0,
       fldTypeString = TString' "CInt",
       fldDims = [],
       isDynamic = False,
       isDirect = False,
       isBitField = False,
       isVariadic = False,
       internId = ""
     }
     putStrLn $ splitEnd

-- Expand declarations so that if there are multiple member declarations
-- of the same type, the same number of one-member declarations will be created.

expdecls sds = concat $ map expdecls' sds where
  expdecls' (StructDecl dss []) = []
  expdecls' (StructDecl dss (sdd:sdds)) = (StructDecl dss [sdd]):(expdecls' (StructDecl dss sdds)) 

-- Convert a structure/union name to Haskell type name.

convname ('s':'t':'r':'u':'c':'t':'@':strname) = "S_" ++ strname
convname ('u':'n':'i':'o':'n':'@':strname) = "U_" ++ strname

-- Restore true structure/union/typedef name for use with hsc2hs.
-- If the structure name ends with a prime, then this is a typedef,
-- and the word struct/union along with the prime must be removed.
-- If it does not end with a prime, @ is replaced with space.

truename strname
  | head (reverse strname) == '\'' = reverse (drop 1 (reverse (drop 2 (convname strname))))
  | otherwise = map at2space strname
      where at2space '@' = ' '
            at2space z = z

-- Write the instance of the field access class for this structure.
-- For direct structures/unions (fldtype starts with '@') a pointer
-- will be retrieved, otherwise a value. For function pointers,
-- a pointer factory will be created and applied, so ready to use
-- function (in Haskell sense) will be returned.
-- For anonymous structures, their C syntax (deparsed) will be used
-- to form the #peek construction.

structinstance fn strinfo fldinfo | take 3 (fldName fldinfo) == "_@_" = return ()

structinstance fn strinfo fldinfo = do
  let icid = (internId fldinfo) ++ (convName strinfo)
      mkfld = icid ++ "___mk"
      wrfld = icid ++ "___wr"
      arglist = intlv (take (fldArity fldinfo) $ map (('_' :) . show) [1..]) " "
      exclimp = case (isDynamic fldinfo, isVariadic fldinfo||isDirect fldinfo) of
                  (False, _)    -> False
                  (True, False) -> False
                  (True, True)  -> True
  instheader fldclass strinfo fldinfo instType "V_"
  case exclimp of
    True -> excludeimport (fldName fldinfo) (isVariadic fldinfo) (isDirect fldinfo)
    False -> case (isBitField fldinfo) of
      True -> bitfield strinfo fldinfo
      False -> do 
        case (isDynamic fldinfo) of
          False -> case (isDirect fldinfo) of
            True  -> ptrfield strinfo (fldName fldinfo)
            False -> case (fldName fldinfo == "sizeof") of
              True  -> sizefield strinfo
              False -> peekfield strinfo (fldName fldinfo)
          True  -> do peekdynfld strinfo (fldName fldinfo) mkfld wrfld
                      makedynfld fldinfo mkfld
                      makewrpfld fldinfo wrfld
                      cbckimport ((convName strinfo) ++ "_" ++ (fldName fldinfo)) 
                                 (fldTypeString fldinfo)
                      when (fldArity fldinfo > 0) $ do
                        instheader fldclass strinfo fldinfo instType "X_"
                        putStrLn $ "  z ==> X_" ++ (fldName fldinfo) ++ " = \\" ++ 
                                   arglist ++ " -> do"
                        putStrLn $ "    x <- z --> V_" ++ (fldName fldinfo) 
                        putStrLn $ "    r <- x " ++ arglist
                        putStrLn $ "    return r"
        when ((length $ fldDims fldinfo) > 0) $ dimfield strinfo fldinfo

-- Output an instance header for the given structure, field, type.

instheader fc si fi ft pfx = 
  putStrLn $ "\ninstance " ++ fc ++ " " ++ (convName si) ++ " (" ++
             (ft fi) ++ ") " ++ pfx ++ (fldName fi) ++ " where"

-- For structures members which are arrays, output 
-- a pseudo-member to access the dimensions

dimfield si fi = do
  instheader fldclass si fi (\_ -> "[Int]") "D_"
  putStrLn $ "  z --> D_" ++ (fldName fi) ++ " = return " ++ (fldDims fi)
  putStrLn $
    "  (z, D_" ++ (fldName fi) ++ ") <-- v = error $ \"dimensions of a field  cannot be set\""

-- Output code to access a bit field.

bitfield si fi = do
  let icid = (internId fi) ++ (convName si)
      getbf = icid ++ "___get___" ++ (fldName fi) ++ "___"
      setbf = icid ++ "___set___" ++ (fldName fi) ++ "___"
      getbfhs = getbf ++ "___hs___"
      setbfhs = setbf ++ "___hs___"
      ctype = map u2sp $ takeWhile (/= '|') (fldType fi)
      u2sp '_' = ' '
      u2sp z = z
  putStrLn $ "  z --> V_" ++ (fldName fi) ++ " = " ++ getbfhs ++ " z"
  putStrLn $ "  (z, V_" ++ (fldName fi) ++ ") <-- v = " ++ setbfhs ++ " z v"
  putStrLn $ ""
  putStrLn $ "foreign import ccall unsafe \"static " ++ getbf ++ "\""
  putStrLn $ "  " ++ getbfhs ++ " :: Ptr " ++ (convName si) ++
             " -> IO " ++ (instType fi)
  putStrLn $ "foreign import ccall unsafe \"static " ++ setbf ++ "\""
  putStrLn $ "  " ++ setbfhs ++ " :: Ptr " ++ (convName si) ++
             " -> " ++ (instType fi) ++ " -> IO ()"
  putStrLn $ ""
  putStrLn $ "#def inline " ++ ctype ++ " " ++ getbf ++ "(void *s) {"
  putStrLn $ "  return ((" ++ (trueName si) ++ " *)s) -> " ++ (fldName fi) ++ ";"
  putStrLn $ "}"
  putStrLn $ ""
  putStrLn $ "#def inline void " ++ setbf ++
             "(void *s, " ++ ctype ++ " v) {"
  putStrLn $ "  ((" ++ (trueName si) ++ " *)s) -> " ++ (fldName fi) ++ " = v;"
  putStrLn $ "}"

ptrfield _ ('_':'@':'_':_) = return ()

ptrfield si fld = do
  putStrLn $ 
    "  z --> V_" ++ fld ++ " = return $ (#ptr __quote__(" ++ (trueName si) ++ "), " ++ fld ++ ") z"
  putStrLn $
    "  (z, V_" ++ fld ++ ") <-- v = error $ \"field " ++ fld ++ " is a structure or an array:" 
                                                      ++ " cannot be set\""

-- Write the V_sizeof field.
-- Also write a instance Storable for the structure. Only sizeOf and alignment
-- are effective. Peek and poke will cause error if used. 

sizefield si = do
  putStrLn $ "  z --> V_sizeof = return $ (#size __quote__(" ++ (trueName si) ++ "))"
  putStrLn $ ""
  putStrLn $ "instance Storable " ++ (convName si) ++ " where"
  putStrLn $ "  sizeOf _ = (#size __quote__(" ++ (trueName si) ++ "))"
  putStrLn $ "  alignment _ = 1"
  putStrLn $ "  peek _ = error $ \"peek and poke cannot be used with " ++ (trueName si) ++ "\""
  putStrLn $ "  poke _ = error $ \"peek and poke cannot be used with " ++ (trueName si) ++ "\""

-- A regular structure member which can be read and set.

peekfield si fld = do
  putStrLn $ "  z --> V_" ++ fld ++ " = (#peek __quote__(" ++ (trueName si) ++ "), " ++ fld ++ ") z"
  putStrLn $ "  (z, V_" ++ fld ++ ") <-- v = (#poke __quote__(" ++ (trueName si) ++ "), " ++ 
             fld ++ ") z v"

-- Output dynamic wrappers for a structure member holding function pointer.

makedynfld fldinfo mkf = do
  putStrLn $ "foreign import ccall \"dynamic\"\n" ++
             "  " ++ mkf ++ " :: (" ++ (fldType fldinfo) ++ ") -> (" ++ (instType fldinfo) ++ ")"

makewrpfld fldinfo wrp  = do
  putStrLn $ "foreign import ccall \"wrapper\"\n" ++
             "  " ++ wrp ++ " :: (" ++ (instType fldinfo) ++ ") -> IO (" ++ (fldType fldinfo) ++ ")"

-- Output code to access a structure member containing a function pointer.

peekdynfld si fld mkf wrp = do
  putStrLn $ "  z --> V_" ++ fld ++ " = (#peek __quote__(" ++ (trueName si) ++ "), " ++ 
             fld ++ ") z" ++
             " >>= (return . " ++ mkf ++ ")"
  putStrLn $ "  (z, V_" ++ fld ++ ") <-- v = (" ++ wrp ++ " v) >>= " ++
             "(#poke __quote__(" ++ (trueName si) ++ "), " ++ fld ++ ") z"


-- Write newtype statements for every structure.

structnewtype strname = do
  putStrLn $ "newtype " ++ (convname strname) ++ " = " ++ (convname strname) ++ " ()"

-- Write type declarations for all type aliases.

tdefalias tymap (tal, DictDecl (Declaration dss [id] _)) = do
  let target = (ts2ts . 
                (monadify "IO") . 
                mapc2hs . 
                state2ts . 
                dcl2ts . 
                (connectta tymap)) (simplifydecl dss id)
      polish ('@':s) = s
      polish z = z
  putStrLn $ "type T_" ++ tal ++ " = " ++ (polish target)

-- Write type equivalencies created during fixing named-within-anonymous struct
-- declarations.

tdefalias tymap (tal, DictTypeEq trg) =
  putStrLn $ "type " ++ tal ++ " = " ++ trg


-- Similarly to Declaration, simplify a structure/union member declaration.

simplifystructdecl (StructDecl dss [sd]) = simplifydecl (fst isd) (snd isd) where
  isd = initdecl dss sd
  initdecl dss (StructDeclarator (Just d) Nothing) = (dss, InitDeclarator d Nothing)
  initdecl _ (StructDeclarator (Just d) (Just c)) = 
    ((DeclSpecType (TypeSpecPrim "$BF$")):dss, InitDeclarator d Nothing)


---------------------------------------------------------------------------------------

-- For every standalone (declared at top level) function or variable
-- (i. e. an unary function), unless it returns a structure.
-- To determine whether a function returns a structure rather than a pointer,
-- its return type alias (if any) is traced back to the original type.
-- If include file name is provided, it will be used in FFI declarations.

inclname (Just fn) = fn
inclname Nothing = ""

writeStandaloneFunctions tus tymap fn = 
  do let imps = map ((finalizeModuleName fn) ++) ["_S", "_C", "_E", "_S_d","_S_t","_S_n"]
         fmfnf = (finalizeModuleName fn) ++ "_F"
     putStrLn $ "#include <stdlib.h>"
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfnf ++ "\n"
     writeSplitHeader imps fmfnf
     mapM (onefunc' tymap (inclname fn)) tus
     putStrLn $ "\n" ++ splitEnd ++ "\n"
     return ()

-- One TransUnitDecl may contain multiple InitDeclarator's.
-- Take care on those assuming they all have the same return
-- type and attributes.

onefunc' tymap ifn (TransUnitDecl (Declaration dss ids ats)) = 
  do mapM (onefunc tymap ifn (typeonly dss) ats) ids
     return ()

onefunc' _ _ _ = return ()

-- Keep only primitive types and type aliases.

typeonly dss = filter typeonly' dss where
  typeonly' (DeclSpecType _) = True
  typeonly' _ = False

-- Retrieve name from an InitDeclarator. This is taken from (Left s)
-- of the last unnested declarator.


dcl2name (Declarator _ (Left s) _) = s
dcl2name (Declarator _ (Right d') _) = dcl2name d'

id2name (InitDeclarator d _) = dcl2name d

-- Retrieve the return type from a declaration as a list of type name
-- strings. Enums are represented as integers.

ds2rtn [] = [] 
ds2rtn (d:ds) = (ds2str d):(ds2rtn ds)
ds2str (DeclSpecType (TypeSpecPrim s)) = s
ds2str (DeclSpecType (TypeSpecAlias s)) = s
ds2str (DeclSpecType (TypeSpecStruct (StructSpec s1 "" _))) = 
  error $ "unfixed declaration of anonymous " ++ s1
ds2str (DeclSpecType (TypeSpecStruct (StructSpec s1 s2 _))) = s1 ++ "_" ++ s2
ds2str (DeclSpecType (TypeSpecEnum (EnumSpec "" _))) = 
  error $ "unfixed declaration of anonymous enum"
ds2str (DeclSpecType (TypeSpecEnum (EnumSpec s _))) = "int"
ds2str z = error $ "ds2str: " ++ (show z)

-- Simplify a Declaration by keeping only type declarations, Declarators
-- without initializers, etc.

data DeclarationS = DeclarationS [String]            -- remains of [DeclSpec]
                                 DeclaratorS         -- remains of InitDeclarator
                                 deriving (Show)

data DeclaratorS = DeclaratorS Int                   -- pointer depth
                               (Maybe DeclaratorS)   -- if "Right Declarator"
                               DeclType              -- remains of CPI
                               deriving (Show)

data DeclType = DeclTypeVar                          -- for variables
              | DeclTypeVariadic                     -- to signal a variadic function
              | DeclTypeFixed [DeclarationS]         -- converted CPIs for regular functions
              | DeclTypeUnknown String               -- not implemented yet
              deriving (Show)

simplifydecl dss id@(InitDeclarator decl mbi) = 
  DeclarationS dsn' (simplifyid $ InitDeclarator decl' mbi) where
    dsn = ds2rtn $ typeonly dss
    (dsn', decl') = convarray (dsn, decl)

-- Convert array types with empty brackets to pointers, i. e. a[][] into int **a.
-- Arrays with nonempty dimension information keep that information in their
-- typestrings.

convarray (tss, d@(Declarator ps esd [])) = (tss, d)

convarray (tss, d@(Declarator ps esd (cpi:cpis))) = 
  case cpi of
    CPICon Nothing -> convarray (tss, Declarator (ps ++ [Pointer []]) esd cpis)
    CPICon (Just con) -> convarray (con2num con : tss, Declarator ps esd cpis)
    other -> (tss, d)

-- When dealing with array dimensions (which may be C constant expressions),
-- wrap them in the #const macro for hsc2hs to process. Replace underscores
-- (_) with backquotes (`) temporarily not to confuse the further processing
-- of the type because underscores serve as separators between parts of the type
-- definition.

con2num con = "(#const(" ++ map un2bq (show con) ++ "))@" where
  un2bq '_' = '`'
  un2bq z = z

convdims d@(Declarator ps esd []) = d

convdims d@(Declarator ps esd (cpi:cpis)) = 
  case cpi of
    (CPICon Nothing) -> convdims (Declarator (ps ++ [Pointer []]) esd cpis)
    other      -> d

-- simplify InitDeclarator (convert to DeclaratorS).

simplifyid (InitDeclarator dd _) = 
  mkdecls (convdims dd) where
    mkdecls (Declarator ps esd cpis) = 
      DeclaratorS (length ps) (convesd esd) (convcpis cpis)
    convesd (Left _) = Nothing
    convesd (Right dd') = 
      Just (mkdecls (convdims dd'))
    convcpis [] = DeclTypeVar
    convcpis [CPIEmpty] = DeclTypeFixed []
    convcpis [CPIPar _ Variadic] = DeclTypeVariadic
    convcpis [CPIPar pds Fixed] = DeclTypeFixed (map convpdecl pds)
    convcpis z = DeclTypeUnknown (show z)
    convpdecl (ParamDecl pdds mbd) = simplifydecl pdds (mbd2id mbd)
    mbd2id (Just d) = InitDeclarator d Nothing
    mbd2id Nothing = InitDeclarator (Declarator [] (Left "") []) Nothing

-- Connect type aliases to the declarations that use them.

-- Apply connection with type aliases to every function parameter type

ctad tymap (DeclaratorS ps mbds dds) = 
  DeclaratorS ps mbds' dds'
    where mbds' = case mbds of
            Just d -> Just $ ctad tymap d 
            other -> mbds
          dds' = case dds of
            DeclTypeFixed dtds -> DeclTypeFixed $ map (connectta tymap) dtds
            other -> dds

-- Follow the type alias' chain of declarators, and when Nothing is found,
-- connect the target declarator (taken from the target declaration).

cncd tymap (DeclarationS rts decl) dclc = DeclarationS rts (cncd' (ctad tymap decl) dclc) where
  cncd' (DeclaratorS ps (Just d) dt) dclc = DeclaratorS ps (Just (cncd' d dclc)) (tpmap dt)
  cncd' (DeclaratorS ps Nothing dt) dclc = DeclaratorS ps (Just dclc) (tpmap dt)
  tpmap dtt = case dtt of
    DeclTypeFixed dtds -> DeclTypeFixed $ map (connectta tymap) dtds
    other -> dtt

-- When mapping type aliases, take care on possible array dimensions:
-- strip them before mapping (they look like n@ where n is a natural number),
-- and prepend them back after the mapping is done.

connectta tymap (DeclarationS rts decl) = 
  restoredims rtsdims $ connectta' tymap (DeclarationS rts' decl) where
    rtsdims = takeWhile ("@" `isSuffixOf`) rts
    rts' = dropWhile ("@" `isSuffixOf`) rts
    restoredims dms (DeclarationS r d) = DeclarationS (dms ++ r) d
    connectta' tymap (DeclarationS rts decl) = 
      let decl' = ctad tymap decl in
        case (length rts) of
          1 -> case (Map.lookup (head rts) tymap) of
                 Nothing -> DeclarationS rts decl'
                 Just (DictDecl (Declaration adss [aid] at)) -> 
                   cncd tymap (connectta tymap $ simplifydecl adss aid) decl'
                 other -> error $ (head rts) ++ " is not a type alias"
          other -> DeclarationS rts decl'

-- Type conversion state machine. The chain of DeclaratorS's starting at the first
-- DeclarationS is followed. Each DeclaratorS acts as an instruction modifying
-- the state.

data TypeString = TString String
                | TString' String	  -- same as TString but no more type mappings
                | TApply  [TypeString]
                | PtrF    Int TypeString
                | PtrV    Int TypeString
                | Mnd     String TypeString
                deriving (Show)

data TCMState = TCMState TypeString        -- current type string
                         Bool              -- True if function (to produce proper Ptr)
                         deriving (Show)

-- String representation of a TypeString

ts2ts (TString s) = s
ts2ts (TString' s) = s
ts2ts (TApply tss) = intlv (map ts2ts tss) " -> "
ts2ts (PtrV ps ts) = nptrs ps (ts2ts ts) "Ptr"
ts2ts (PtrF ps ts) = nptrs ps (ts2ts ts) "FunPtr"
ts2ts (Mnd  ms (TString ts)) = ms ++ " " ++ ts
ts2ts (Mnd  ms z) = ms ++ " (" ++ ts2ts z ++ ")"

nptrs 0 s _  = s
nptrs n s p = p ++ " (" ++ (nptrs (n - 1) s p) ++ ")"

-- Map C types in the type string to Haskell types (if available).

mapc2hs (TString' z) = TString' z

-- Function application: map type of each parameter and return type.
-- Take arguments-arrays into consideration.

mapc2hs (TApply tss) = TApply $ map mapc2hs_arr tss

-- Special cases.

-- Pointer types: map the target type.
-- Nested pointers: increase pointer depth. 
-- Pointers to void are represented as Ptr CChar.
-- Pointers to variadic functions are represented as pointers to nullary
-- functions.
-- Structures, unions: only pointers are valid.

mapc2hs (PtrF ps ts) = case ts of 
  PtrF ps' ts' -> mapc2hs (PtrF (ps + ps') ts')
  (TApply [TString "@@variadic@@"]) -> PtrF ps (TApply [TString "()"])
  other -> PtrF ps (mapc2hs ts) 

mapc2hs (PtrV ps ts) = case ts of
  PtrV ps' ts' -> mapc2hs (PtrV (ps + ps') ts')
  TString "void" -> mapc2hs (PtrV ps (TString "char"))
  TString ('s':'t':'r':'u':'c':'t':'_':strname) -> PtrV ps (TString ("S_" ++ strname))
  TString ('u':'n':'i':'o':'n':'_':strname) -> PtrV ps (TString ("U_" ++ strname))
  other -> PtrV ps (mapc2hs ts) 

-- Manually hardcoded type mapping, based on Page 32 of the FFI Addendum.

mapc2hs (TString "int") = TString' "CInt"
mapc2hs (TString "signed_int") = TString' "CInt"
mapc2hs (TString "unsigned_int") = TString' "CUInt"
mapc2hs (TString "signed") = TString' "CInt"
mapc2hs (TString "unsigned") = TString' "CUInt"
mapc2hs (TString "short") = TString' "CShort"
mapc2hs (TString "unsigned_short") = TString' "CUShort"
mapc2hs (TString "short_int") = TString' "CShort"
mapc2hs (TString "signed_short_int") = TString' "CShort"
mapc2hs (TString "unsigned_short_int") = TString' "CUShort"
mapc2hs (TString "char") = TString' "CChar"
mapc2hs (TString "signed_char") = TString' "CSChar"
mapc2hs (TString "unsigned_char") = TString' "CUChar"
mapc2hs (TString "long") = TString' "CLong"
mapc2hs (TString "unsigned_long") = TString' "CULong"
mapc2hs (TString "long_int") = TString' "CLong"
mapc2hs (TString "unsigned_long_int") = TString' "CULong"
mapc2hs (TString "long_long") = TString' "CLLong"
mapc2hs (TString "unsigned_long_long") = TString' "CULLong"
mapc2hs (TString "long_long_int") = TString' "CLLong" 
mapc2hs (TString "signed_long_long_int") = TString' "CLLong"
mapc2hs (TString "unsigned_long_long_int") = TString' "CULLong"
mapc2hs (TString "float") = TString' "CFloat"
mapc2hs (TString "double") = TString' "CDouble"
mapc2hs (TString "long_double") = TString' "CLDouble"
mapc2hs (TString "@@ptrdiff_t@@") = TString' "CPtrdiff"
mapc2hs (TString "@@size_t@@") = TString' "CSize"
mapc2hs (TString "@@wchar_t@@") = TString' "CWchar"
mapc2hs (TString "@@sig_atomic_t@@") = TString' "CSigAtomic"
mapc2hs (TString "@@clock_t@@") = TString' "CClock"
mapc2hs (TString "@@time_t@@") = TString' "CTime"
mapc2hs (TString "@@FILE@@") = TString' "CFile"
mapc2hs (TString "@@fpos_t@@") = TString' "CFpos"
mapc2hs (TString "@@jmp_buf@@") = TString' "CJmpBuf"
mapc2hs (TString "@@void@@") = TString' "()"
mapc2hs (TString "void") = TString' "()"
mapc2hs (TString "@@variadic@@") = TString' "WrongVariadicFunction"

-- Direct structures/unions: valid in some circumstances, but
-- require special treatment.

mapc2hs (TString ('s':'t':'r':'u':'c':'t':'_':strname)) =
  TString' ("@S_" ++ strname)

mapc2hs (TString ('u':'n':'i':'o':'n':'_':strname)) =
  TString' ("@U_" ++ strname)

-- Special pseudo-type for bit fields.

mapc2hs (TString ('$':'B':'F':'$':'_':s)) = TString' (s ++ "|" ++ ts2ts (mapc2hs (TString s)))

-- Array types: represented as pointers to the type of array element.
-- Array type string starts with a digit and contains @-sign at non-head
-- position.
-- The rest, will be converted into unknown types, and will cause
-- compilation error.

mapc2hs (TString at)
  | isarrt at = mapc2hs (TString $ snd (splitarrt at))
  | otherwise = unmapped at

-- Check whether the typestring represents an array type.

isarrt at = let firstdim = takeWhile (/= '@') at in
            (length firstdim > 0) && ("(#const" `isPrefixOf` firstdim)

-- Special version of the type map function considering conversion of arrays
-- into pointers.

mapc2hs_arr t@(TString at)
  | isarrt at = PtrV 1 (mapc2hs (TString $ snd (splitarrt at)))
  | otherwise = mapc2hs t

mapc2hs_arr z = mapc2hs z

unmapped z = TString' ("Unmapped_C_Type_" ++ z)

-- Split array type string into dimensions ([Int]) and the base type string
-- (String).

splitarrt arts = (dims,basetype)
  where notat = (/= '@')
        basetype = (drop 1 . reverse . takeWhile notat . reverse) arts
        dimtxt = (reverse . dropWhile notat . reverse) arts
        dimpts = parts (== '_') dimtxt
        dims = map bq2un dims'
        bq2un '`' = '_'
        bq2un z = z
        dims' = "[" ++ intlv (reverse $ map (readdim . filter notat) dimpts) ", " ++ "]"
        readdim "*" = "-1"
        readdim s = s
        

-- Monadify the type string. All functions must return
-- monadic (usually IO) types.

monadify m (TApply tss) = 
  TApply (reverse $ (Mnd m (monadify m (head rtss))):(map (monadify m) $ tail rtss))
    where rtss = reverse tss

monadify m (PtrF ps ts) = PtrF ps (monadify m ts)

monadify m (PtrV ps ts) = PtrV ps (monadify m ts)

monadify m z = z

-- Pointer application. First pointer uses the prefix string, the rest
-- just add Ptr to the type string. A pointer of zero depth acts as a pair
-- of parentheses. The prefix string is cleared is at least one pointer was applied.

ptrapply 0 st = st

ptrapply 1 (TCMState curts pfx) = TCMState (ptrapply' curts pfx) False where
  ptrapply' t@(PtrF ps ts)   False = PtrV 1 t
  ptrapply'   (PtrV ps ts)   False = PtrV (ps + 1) ts
  ptrapply' t@(TString ts)   False = PtrV 1 t
  ptrapply' t                True  = PtrF 1 t
  ptrapply' x y = error $ "ptrapply' " ++ show x ++ " " ++ show y

ptrapply ps st = ptrapply (ps - 1) $ clrpfx $ ptrapply 1 st where
  clrpfx (TCMState curts _) = TCMState curts False

-- Parameters application. List of declarations is converted into
-- type strings, and they are interleaved with an arrow. All this
-- is appended along with an arrow to the left of the type string.

parmsapply dcls (TCMState curts _) = 
  TCMState (tsconcat (map dcl2ts' dcls) curts) True where
    dcl2ts' dcl = state2ts $ dcl2ts dcl
    tsconcat [TString "void"] tr = TApply [tr]
    tsconcat tp tr = TApply (tp ++ [tr])

-- Applies a pointer to the current type string.

tcmtrans st (DeclaratorS ps _ DeclTypeVar) = ptrapply ps st

-- Inserts function parameters' type signatures (return type was in DeclarationS)
-- If pc is not zero, applies a pointer ps times to what is in the type string.

tcmtrans st (DeclaratorS ps _ (DeclTypeFixed dcls)) = parmsapply dcls $ ptrapply ps st

-- Variadic functions are not supported. Yet error cannot be declared here
-- as they shouldn't harm others.

tcmtrans st (DeclaratorS _ _ DeclTypeVariadic) = 
  TCMState (TApply [TString "@@variadic@@"]) True

tcmtrans st (DeclaratorS _ _ (DeclTypeUnknown t)) = 
  TCMState (TApply [TString $ "@@unknown" ++ t ++ "@@"]) True

-- Initializes a type string from a DeclarationS

dcl2state (DeclarationS ds _) = TCMState (TString (intlv ds "_")) False

-- Retrieves type string from a state.

state2ts (TCMState ts _) = ts

-- Determines whether the state represents a function.

state2isf (TCMState _ pfx) = pfx

-- Follows the chain of declarators.

dclfollow st d@(DeclaratorS _ mbds _) = 
  let st' = tcmtrans st d in
    case mbds of
      Nothing -> st'
      Just d' -> dclfollow st' d'

-- Converts a declaration into a type string.

dcl2ts d@(DeclarationS ds decl) = dclfollow (dcl2state d) decl

-- Check recursively a TypeString for a predicate. Return True if at least
-- one element of the TypeString satisfies.

checktsrec pred ta@(TApply tss) = (pred ta) || (foldl (||) False (map (checktsrec pred) tss)) 
checktsrec pred pv@(PtrV _ ts) = (pred pv) || (checktsrec pred ts)
checktsrec pred pf@(PtrF _ ts) = (pred pf) || (checktsrec pred ts)
checktsrec pred mn@(Mnd _ ts) = (pred mn) || (checktsrec pred ts)
checktsrec pred z = pred z

-- True if a type is of a variadic function.

isvariadic ts = checktsrec isvr ts where
  isvr (TString  "WrongVariadicFunction") = True
  isvr (TString' "WrongVariadicFunction") = True
  isvr _ = False

-- True if a type is of a function taking/returning direct structures

isdirstruct ts = checktsrec isd ts where
  isd (TString  ('@':_)) = True
  isd (TString' ('@':_)) = True
  isd _ = False

-- Output a FFI declaration of a function or a variable.
-- There may be following kinds of things declared:
--  - variables (&-import)
--  - regular functions (static import)
--  - pointers to functions: dynamic import for declaration, wrapper import for arguments
--  - pointers to variables
--  - pointers to pointers to functions
-- These things may be declared in two ways:
--  - type/typealias id (variables, functions, pointers)
--  - rettype/retalias id args (functions, function pointers)
-- In the first case, typealias resolution is necessary to determine whether
-- a function or a variable is declared. In the second case, presence of parameters
-- in the declaration shows whether this is a function (maybe nullary) or a variable.

onefunc tymap ifn dss ats id = 
  do let cta = connectta tymap $ simplifydecl dss id
         tcm = dcl2ts cta
         isf = state2isf tcm
         isv = isvariadic tsp
         drs = isdirstruct tsp
         dyn = case (state2ts tcm) of
               PtrF _ _ -> True
               other -> False
         tsp = mapc2hs $ state2ts tcm
         tsi = monadify "IO" tsp
         tsf = ts2ts tsi
         tsg = ts2ts tsp
         sym = id2name id
         intern = case (Map.lookup ("ID " ++ sym) tymap) of
                  Just (DictId n) -> "___" ++ (show n) ++ "___"
                  other -> ""
         arity (TApply ts) = (length ts) - 1
         arity (PtrF _ t) = arity t
         arity _ = 0
     case (isv || drs,isf,dyn) of
       (False,True, False) -> do statimport ifn sym tsf
                                 cbckimport sym tsi
       (_,    True, True)  -> skipimport ifn sym
       (False,False,True)  -> do dynimport  ifn sym tsf (arity tsp) intern
                                 cbckimport sym tsi
       (False,False,False) -> varimport  ifn sym tsg True intern
       (True, _,    _)     -> excludeimport (sym ++ " :: " ++ tsg) isv drs

     putStrLn ""
     putStrLn "--"
     putStrLn ""
     return ()

-- Exclude an import and explain the reason.

excludeimport sym isv drs = do
  putStrLn   "--"
  putStrLn $ "-- import of function/variable/structure member(s) " ++ sym ++ " is not possible"
  putStrLn $ "-- because of the following reason(s):"
  when isv $ putStrLn "-- function is variadic"
  when drs $ putStrLn "-- function takes/returns structure(s) directly"
  putStrLn   "--"
                  

-- Write an import statement for a function. The `alloca' function if not really a function,
-- so import is not written for it.

statimport _ "alloca" _ = return ()

statimport ifn sym tsg = do
  putStrLn $ "foreign import ccall \"static " ++ ifn ++ " " ++ sym ++ "\""
  putStrLn $ "  f_" ++ sym ++ " :: " ++ tsg

-- For a dynamic import, 3 declarations are made: the first for the variable
-- holding a function pointer, the second for the stub factory, and the third
-- is application of the factory to the pointer variable (this one has name
-- of the imported entity.

-- For convenience, the function pointer to call will be imported
-- under the pointer variable's name, such as (for a function pointer
-- double (*pdf)(double,double)):

-- foreign import ccall "example.h &pdf" 
--   pdf' :: Ptr (FunPtr (CDouble -> CDouble -> IO CDouble))
-- foreign import ccall "dynamic"
--   mk_pdf' :: FunPtr (CDouble -> CDouble -> IO CDouble) -> (CDouble -> CDouble -> IO CDouble)
-- pdf _1 _2 = peek pdf' >>= \s -> mk_pdf' s _1 _2

unfptr ('F':'u':'n':'P':'t':'r':' ':s) = s
unfptr z = z

dynimport ifn sym tsg arity intern = do
  let csym = "x_" ++ sym
      vsym = "v_" ++ sym
      ssym = "s_" ++ sym
      msym = intern ++ "mk___"
      wsym = intern ++ "wr___"
      gsym = "peek " ++ intern
      arglist = intlv (take arity $ map (('_' :) . show) [1..]) " "
  varimport ifn sym tsg False intern
  putStrLn $ "foreign import ccall \"dynamic\""
  putStrLn $ "  " ++ msym ++ " :: " ++ tsg ++ " -> " ++ (unfptr tsg)
  putStrLn $ "foreign import ccall \"wrapper\""
  putStrLn $ "  " ++ wsym ++ " :: " ++ (unfptr tsg) ++ " -> " ++ "IO (" ++ tsg ++ ")"
  putStrLn $ csym ++ " " ++ arglist ++ " = " ++ gsym ++ " >>= \\s -> " ++ msym ++ " s " ++ arglist
  putStrLn $ vsym ++ " = " ++ gsym ++ " >>= (return . " ++ msym ++ ")"
  putStrLn $ ssym ++ " = \\s -> " ++ wsym ++ " s >>= poke " ++ intern
  return ()

-- Write a comment about ambiguously defined import entity.

skipimport ifn sym = do
  putStrLn $ "-- Import generation ERROR"
  putStrLn $ "-- Import of " ++ sym ++ " defined in " ++ ifn ++ " is skipped:"
  putStrLn $ "-- it is ambiguously declared as a dynamically imported function"
  putStrLn $ "-- rather than a variable pointing to a function."

-- Write an import statement for a variable. For convenience,
-- for each variable, a function will be defined to access variable's
-- value, such as (for an integer variable `a'):

-- Thus the pointer to the variable is imported under the name composed of
-- variable name and a prime.

varimport ifn sym tsg getset intern = do
  putStrLn $ "foreign import ccall \"" ++ ifn ++ " &" ++ sym ++ "\" "
  putStrLn $ "  " ++ intern ++ " :: Ptr (" ++ tsg ++ ")"
  putStrLn $ "p_" ++ sym ++ " = " ++ intern
  case getset of
    True -> do putStrLn $ "v_" ++ sym ++ " = peek " ++ intern
               putStrLn $ "s_" ++ sym ++ " = poke " ++ intern
    False -> return ()
  return ()

-- Recursively scan the type for funciton pointers passed as arguments
-- and write import statements to define wrappers.


findcbcks (TApply tss) = concat $ map findcbcks (reverse $ tail $ reverse tss)

findcbcks (PtrF 1 ts) = [ts2ts ts] ++ (findcbcks ts)

findcbcks z = []

cbckimport sym tsp = do
  let cbl = zip [1..] (nub $ findcbcks $ ftsg tsp)
      ftsg t@(TApply _) = t
      ftsg (PtrF _ t@(TApply _)) = t
      ftsg _ = TApply []
  mapM (wrapimport sym) cbl
  return ()

-- Write one wrapper import statement. Each wrapper gets a name
-- derived from sym and its position in the list.

wrapimport sym cb = when ((length $ snd cb) > 0) $ do
  putStrLn $ "foreign import ccall \"wrapper\""
  putStrLn $ "  " ++ "w_" ++ sym ++ "_" ++ (show $ fst cb) ++ " :: " 
                  ++ "(" ++ (snd cb) ++ ") -> IO (FunPtr (" ++ (snd cb) ++ "))"
  return ()

