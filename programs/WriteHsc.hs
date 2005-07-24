-- Output contents of a .hsc file to standard output

module WriteHsc where

import Char
import C_BNF
import SplitBounds
import Template
import System.Cmd
import System.Exit
import Data.FiniteMap
import Data.Maybe
import Data.List
import Control.Monad

-- Name for the HSFFIG field access class, and module to import

fldclass = "HSFFIG.FieldAccess.FieldAccess"
fldmodule = "HSFFIG.FieldAccess"

-- A version of words, but works with any lists on any predicate.

parts pred s = case dropWhile pred s of
                                     [] -> []
                                     s' -> w : parts pred s''
                                         where (w, s'') = break pred s'

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

ghcopts = "{-# OPTIONS -fglasgow-exts -ffi #-}"

writeModHdr mfn = do let fmfn = finalizeModuleName mfn
                     writeTemplate
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
                     putStrLn $ "  module " ++ fmfn ++ "_S_cnt,"
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
                     putStrLn $ "import " ++ fmfn ++ "_S_cnt"
                     putStrLn $ "import " ++ fldmodule
                     putStrLn $ splitClose
                     putStrLn $ "\n" ++ splitEnd ++ "\n"

finalizeFileName Nothing = "\"@@INCLUDEFILE@@\""
finalizeFileName (Just s) = "\""++s++"\""

finalizeModuleName Nothing = "@@MODULENAME@@"
finalizeModuleName (Just s) = map d2uu (head $ reverse $ parts (== '/') s)
                                where d2uu c
                                        | c == '.' = '_'
                                        | isAlpha c = toUpper c
                                        | otherwise = c 

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

writeConstAccess tus Nothing = return ()
writeConstAccess tus (Just fn) = 
  do let cnsts = keysFM $ filterFM constonly tus
         constonly _ DictDef = True
         constonly _ _ = False
         fmfnc = (finalizeModuleName (Just fn)) ++ "_C"
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfnc ++ "\n"
     writeSplitHeader [] fmfnc
     mapM (oneconst fn) cnsts 
     putStrLn $ "\n" ++ splitEnd ++ "\n"
     return ()

oneconst fn cnst = 
  do rc <- testconst fn cnst
     case rc of
          ExitSuccess -> putStrLn $ "c_" ++ cnst ++ " = #const " ++ cnst
          _ -> return ()

testconst fn cnst = system cmdline
  where cmdline = "echo '#include " ++ 
                  (finalizeFileName (Just fn)) ++
                  "\nstatic int a = " ++
                  cnst ++ ";' | gcc -pipe -x c -q -fsyntax-only - 2>/dev/null"

---------------------------------------------------------------------------------------

-- For every enumeration, compute values for implicitly valued variants.
-- Then emit all variants as constants similarly to constants themselves.

writeEnums tus tymap fn =
  do let enums = fmToList $ filterFM enumsonly tymap
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

--  data V_a = V_a deriving (Show)
--  data V_b = V_b deriving (Show)
--  data V_c = V_c deriving (Show)

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

sd2fld (StructDeclarator (Just d) _) = id2name (InitDeclarator d Nothing)
sd2fld _ = ""

collectfields dss = concat $ map collectfields' dss where
  collectfields' (DictStruct su ss) = concat $ map collectfields'' ss where
    collectfields'' (StructDecl _ sds) = map sd2fld sds where

writefields flds fn =
  mapM fldata flds where
    fldata fld = do putStrLn $ "data V_" ++ fld ++ " = V_" ++ fld ++ " deriving (Show)"
                    putStrLn $ "data X_" ++ fld ++ " = X_" ++ fld ++ " deriving (Show)"

writeStructures tus tymap fn = 
  do let structs = filterFM structonly tymap
         typedefs = fmToList $ filterFM tdefonly tymap
         structonly _ (DictStruct _ _) = True
         structonly _ _ = False
         tdefonly _ (DictDecl _) = True
         tdefonly _ _ = False
         allfields = nub $ collectfields (eltsFM structs)
         fmfns = (finalizeModuleName fn) ++ "_S"
         strpairs = fmToList structs
         strnames = map fst strpairs
         submods = map (((fmfns ++ "_") ++) . convname) strnames
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfns ++ "\n"
     writeSplitHeaderX submods submods fmfns
     putStrLn $ "\n" ++ splitEnd
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ fmfns ++ "_cnt\n"
     writeSplitHeader [] $ fmfns ++ "_cnt"
     writefields ("sizeof" : allfields) fn
     putStrLn ""
     mapM (tdefalias tymap) typedefs
     putStrLn ""
     mapM structnewtype strnames
     putStrLn $ "\n" ++ splitEnd ++ "\n"
     mapM (onestruct tymap fn) strpairs
     putStrLn $ "\n" ++ splitEnd ++ "\n"
     return ()

-- Write complete definition for a single structure/union.

onestruct tymap fn (strname,strdecl) = 
  do let csyntax = show strdecl
         xdecls = decls strdecl
         fields = map (sd2fld . sdcl2sd) xdecls
         sdcl2sd (StructDecl _ [s]) = s
         decls (DictStruct su s) = expdecls s
         isvs = map isvariadic cmaps
         drss = map isdirstruct cmaps
         states = map (dcl2ts . (connectta tymap) . simplifystructdecl) xdecls
         cmaps = map (mapc2hs . state2ts) states
         stsps = map condmonadify cmaps
         sdecls = map (ts2ts . condmonadify) cmaps
         dyns = map (isdyn . state2ts) states
         condmonadify t = if (isdyn t) then (monadify "IO" t) else t
         isdyn (PtrF _ _) = True
         isdyn _ = False
         fldecls = zip6 isvs drss fields dyns sdecls stsps
         sametype (_, _, _, _, s1, _) (_, _, _, _, s2, _) = (s1 == s2) 
         cmptype (_, _, _, _, s1, _) (_, _, _, _, s2, _) = compare s1 s2
         grpdecls = ((groupBy sametype) . (sortBy cmptype)) fldecls
         fmfns = (finalizeModuleName fn) ++ "_S"
         strm = fmfns ++ "_" ++ (convname strname)

     putStrLn ""
     putStrLn "--"
     putStrLn ""
     putStrLn $ "\n" ++ splitBegin ++ "/" ++ strm ++ "\n"
     writeSplitHeader [fmfns ++ "_cnt", fldmodule] strm
     mapM (writegroupfld fn strname csyntax tymap) grpdecls
     when ((length grpdecls) /= 0) $ structinstance fn strname False "CInt" csyntax tymap 0 "sizeof"
     putStrLn $ splitEnd

-- Write declarations for a group of fields of the same type.
-- Import will be excluded if a member is pointer to a variadic
-- function, or if a pointer to a function taking/returning
-- direct structures.

writegroupfld fn strname csyntax tymap grp = 
  do let fields = map fldname grp
         grptype = fldtype $ head grp
         grptsp = fldtsp $ head grp
         grpdyn = flddyn $ head grp
         grpisv = fldisv $ head grp
         grpdrs = flddrs $ head grp
         fldname (_, _, s, _, _, _) = s
         fldtype (_, _, _, _, t, _) = t
         flddyn  (_, _, _, d, _, _) = d
         fldisv  (i, _, _, _, _, _) = i
         flddrs  (_, d, _, _, _, _) = d
         fldtsp  (_, _, _, _, _, p) = p
         arity (TApply ts) = (length ts) - 1
         arity (PtrF _ t) = arity t
         arity _ = 0
         grpary = arity grptsp
         exclimp = case (grpdyn,grpisv||grpdrs) of
                     (False, _)    -> False
                     (True, False) -> False
                     (True, True)  -> True
         qual f = (convname strname) ++ "_" ++ f
     case exclimp of
       True -> do excludeimport (show fields) grpisv grpdrs
                  return ()
       False -> do mapM (\f -> do structinstance fn strname grpdyn grptype csyntax tymap grpary f
                                  when grpdyn $ cbckimport (qual f) grptsp) fields
                   return ()

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

structinstance fn strname flddyn fldtype csyntax tymap ary field = do
  let fldtype' = case flddyn of
        True  -> unfptr fldtype
        False -> case isdirstruct of
          True  -> "Ptr " ++ (drop 1 fldtype)
          False -> case isbitfld of
            True -> drop 1 $ dropWhile (/= '|') fldtype
            False -> fldtype
      intern = case (lookupFM tymap ("ID " ++ field)) of
               Just (DictId n) -> "___" ++ (show n) ++ "___"
               other -> ""
      mkfld = intern ++ (convname strname) ++ "___mk"
      wrfld = intern ++ (convname strname) ++ "___wr"
      isdirstruct = ((head fldtype) == '@')
      isanon ('s':'t':'r':'u':'c':'t':'@':s:_) = isDigit s
      isanon ('u':'n':'i':'o':'n':'@':s:_) = isDigit s
      isanon _ = False
      isbitfld = '|' `elem` fldtype
      truestr = if (isanon strname) then csyntax else (truename strname)
      arglist = intlv (take ary $ map (('_' :) . show) [1..]) " "
  putStrLn $ "\ninstance " ++ fldclass ++ " " ++
             (convname strname) ++ " (" ++ fldtype' ++ ") V_" ++ field ++ " where"
  case isbitfld of
    True -> do let getbf = intern ++ (convname strname) ++ "___get___" ++ field ++ "___"
                   setbf = intern ++ (convname strname) ++ "___set___" ++ field ++ "___"
                   getbfhs = getbf ++ "___hs___"
                   setbfhs = setbf ++ "___hs___"
                   ctype = map u2sp $ takeWhile (/= '|') fldtype
                   u2sp '_' = ' '
                   u2sp z = z
               putStrLn $ "  z --> V_" ++ field ++ " = " ++ getbfhs ++ " z"
               putStrLn $ "  (z, V_" ++ field ++ ") <-- v = " ++ setbfhs ++ " z v"
               putStrLn $ ""
               putStrLn $ "foreign import ccall unsafe \"static " ++ getbf ++ "\""
               putStrLn $ "  " ++ getbfhs ++ " :: Ptr " ++ (convname strname) ++ 
                          " -> IO " ++ fldtype'
               putStrLn $ "foreign import ccall unsafe \"static " ++ setbf ++ "\""
               putStrLn $ "  " ++ setbfhs ++ " :: Ptr " ++ (convname strname) ++ 
                          " -> " ++ fldtype' ++ " -> IO ()"
               putStrLn $ ""
               putStrLn $ "#def inline " ++ ctype ++ " " ++ getbf ++ "(void *s) {"
               putStrLn $ "  return ((" ++ truestr ++ " *)s) -> " ++ field ++ ";"
               putStrLn $ "}"
               putStrLn $ ""
               putStrLn $ "#def inline void " ++ setbf ++ 
                          "(void *s, " ++ ctype ++ " v) {"
               putStrLn $ "  ((" ++ truestr ++ " *)s) -> " ++ field ++ " = v;"
               putStrLn $ "}"
    False ->
      case flddyn of
        False -> case isdirstruct of
          True  -> ptrfield truestr (convname strname) field
          False -> case (field == "sizeof") of
            True  -> sizefield truestr (convname strname)
            False -> peekfield truestr (convname strname) field
        True  -> do peekdynfld truestr (convname strname) field mkfld wrfld
                    makedynfld fldtype fldtype' mkfld
                    makewrpfld fldtype fldtype' wrfld
                    when (ary > 0) $ do
                      putStrLn $ "\ninstance " ++ fldclass ++ " " ++
                                 (convname strname) ++ " (" ++ fldtype' ++ ") X_" ++ 
                                 field ++ " where"
                      putStrLn $ "  z ==> X_" ++ field ++ " = \\" ++ arglist ++ " -> do"
                      putStrLn $ "    x <- z --> V_" ++ field 
                      putStrLn $ "    r <- x " ++ arglist
                      putStrLn $ "    return r"

ptrfield truestr str fld = do
  putStrLn $ 
    "  z --> V_" ++ fld ++ " = return $ (#ptr __quote__(" ++ truestr ++ "), " ++ fld ++ ") z"
  putStrLn $
    "  (z, V_" ++ fld ++ ") <-- v = error $ \"field " ++ fld ++ " is a structure: cannot be set\""

-- Write the V_sizeof field.
-- Also write a instance Storable for the structure. Only sizeOf and alignment
-- are effective. Peek and poke will cause error if used. 

sizefield truestr str = do
  putStrLn $ "  z --> V_sizeof = return $ (#size __quote__(" ++ truestr ++ "))"
  putStrLn $ ""
  putStrLn $ "instance Storable " ++ str ++ " where"
  putStrLn $ "  sizeOf _ = (#size __quote__(" ++ truestr ++ "))"
  putStrLn $ "  alignment _ = 1"
  putStrLn $ "  peek _ = error $ \"peek and poke cannot be used with " ++ truestr ++ "\""
  putStrLn $ "  poke _ = error $ \"peek and poke cannot be used with " ++ truestr ++ "\""

peekfield truestr str fld = do
  putStrLn $ "  z --> V_" ++ fld ++ " = (#peek __quote__(" ++ truestr ++ "), " ++ fld ++ ") z"
  putStrLn $ "  (z, V_" ++ fld ++ ") <-- v = (#poke __quote__(" ++ truestr ++ "), " ++ fld ++ ") z v"

makedynfld fromtype totype mkf = do
  putStrLn $ "foreign import ccall \"dynamic\"\n" ++
             "  " ++ mkf ++ " :: (" ++ fromtype ++ ") -> (" ++ totype ++ ")"


makewrpfld totype fromtype wrp = do
  putStrLn $ "foreign import ccall \"wrapper\"\n" ++
             "  " ++ wrp ++ " :: (" ++ fromtype ++ ") -> IO (" ++ totype ++ ")"

peekdynfld truestr str fld mkf wrp = do
  putStrLn $ "  z --> V_" ++ fld ++ " = (#peek __quote__(" ++ truestr ++ "), " ++ fld ++ ") z" ++
             " >>= (return . " ++ mkf ++ ")"
  putStrLn $ "  (z, V_" ++ fld ++ ") <-- v = (" ++ wrp ++ " v) >>= " ++
             "(#poke __quote__(" ++ truestr ++ "), " ++ fld ++ ") z"


-- Write newtype statements for every structure.

structnewtype strname = do
  putStrLn $ "newtype " ++ (convname strname) ++ " = " ++ (convname strname) ++ " ()"

-- Write type declarations for all type aliases.

tdefalias tymap (tal,DictDecl (Declaration dss [id] _)) = do
  let target = (ts2ts . 
                (monadify "IO") . 
                mapc2hs . 
                state2ts . 
                dcl2ts . 
                (connectta tymap)) (simplifydecl dss id)
      polish ('@':s) = s
      polish z = z
  putStrLn $ "type T_" ++ tal ++ " = " ++ (polish target)

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
  do let imps = map ((finalizeModuleName fn) ++) ["_S", "_C", "_E", "_S_cnt"]
         fmfnf = (finalizeModuleName fn) ++ "_F"
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

-- Interleave a list of strings with a string.

intlv [] _ = ""
intlv [x] _ = x
intlv (rt:rts) s = rt ++ s ++ (intlv rts s)

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

simplifydecl dss id = DeclarationS (ds2rtn $ typeonly dss) (simplifyid id)

-- Convert array types to pointers, i. e. a[][] into int **a.

convdims d@(Declarator ps esd []) = d

convdims d@(Declarator ps esd (cpi:cpis)) = 
  case cpi of
    (CPICon _) -> convdims (Declarator (ps ++ [Pointer []]) esd cpis)
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
    convpdecl (ParamDecl pdds mbd) = DeclarationS (ds2rtn $ typeonly pdds) (convmbd mbd)
    convmbd Nothing = DeclaratorS 0 Nothing DeclTypeVar
    convmbd (Just dd'') = 
      mkdecls (convdims dd'')

-- Connect type aliases to the declarations that use them.

-- apply connection with type aliases to every function parameter type

ctad tymap (DeclaratorS ps mbds (DeclTypeFixed dds)) = 
  DeclaratorS ps mbds (DeclTypeFixed (map (connectta tymap) dds))

ctad _ z = z

-- Follow the type alias' chain of declarators, and when Nothing is found,
-- connect the target declarator (taken from the target declaration).

cncd tymap (DeclarationS rts decl) dclc = DeclarationS rts (cncd' (ctad tymap decl) dclc) where
  cncd' (DeclaratorS ps (Just d) dt) dclc = DeclaratorS ps (Just (cncd' d dclc)) dt
  cncd' (DeclaratorS ps Nothing dt) dclc = DeclaratorS ps (Just dclc) dt

connectta tymap (DeclarationS rts decl) = 
  let decl' = ctad tymap decl in
    case (length rts) of
      1 -> case (lookupFM tymap (head rts)) of
             Nothing -> DeclarationS rts decl'
             Just (DictDecl (Declaration adss [aid] at)) -> 
               cncd tymap (connectta tymap $ simplifydecl adss aid) decl'
             other -> error $ (head rts) ++ " is not a type alias"
      other -> DeclarationS rts decl'

-- Type conversion state machine. The chain of DeclaratorS's starting at the first
-- DeclarationS is followed. Each DeclaratorS acts as an instruction modifying
-- the state.

data TypeString = TString String
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
ts2ts (TApply tss) = intlv (map ts2ts tss) " -> "
ts2ts (PtrV ps ts) = nptrs ps (ts2ts ts) "Ptr"
ts2ts (PtrF ps ts) = nptrs ps (ts2ts ts) "FunPtr"
ts2ts (Mnd  ms (TString ts)) = ms ++ " " ++ ts
ts2ts (Mnd  ms z) = ms ++ " (" ++ ts2ts z ++ ")"

nptrs 0 s _  = s
nptrs n s p = p ++ " (" ++ (nptrs (n - 1) s p) ++ ")"

-- Map C types in the type string to Haskell types (if available).

-- Function application: map type of each parameter and return type.

mapc2hs (TApply tss) = TApply $ map mapc2hs tss

-- Special cases.

-- Pointers to void are represented as Ptr CChar.

mapc2hs (PtrV ps (TString "void")) = mapc2hs (PtrV ps (TString "char"))

-- Pointers to variadic functions are represented as pointers to unary
-- functions.

mapc2hs (PtrF ps (TApply [TString "@@variadic@@"])) = 
  PtrF ps (TApply [TString "()"])

-- Manually hardcoded type mapping, based on Page 32 of the FFI Addendum.

mapc2hs (TString "int") = TString "CInt"
mapc2hs (TString "signed_int") = TString "CInt"
mapc2hs (TString "unsigned_int") = TString "CUInt"
mapc2hs (TString "signed") = TString "CInt"
mapc2hs (TString "unsigned") = TString "CUInt"
mapc2hs (TString "short") = TString "CShort"
mapc2hs (TString "unsigned_short") = TString "CUShort"
mapc2hs (TString "short_int") = TString "CShort"
mapc2hs (TString "signed_short_int") = TString "CShort"
mapc2hs (TString "unsigned_short_int") = TString "CUShort"
mapc2hs (TString "char") = TString "CChar"
mapc2hs (TString "signed_char") = TString "CSChar"
mapc2hs (TString "unsigned_char") = TString "CUChar"
mapc2hs (TString "long") = TString "CLong"
mapc2hs (TString "unsigned_long") = TString "CULong"
mapc2hs (TString "long_int") = TString "CLong"
mapc2hs (TString "unsigned_long_int") = TString "CULong"
mapc2hs (TString "long_long") = TString "CLLong"
mapc2hs (TString "unsigned_long_long") = TString "CULLong"
mapc2hs (TString "long_long_int") = TString "CLLong" 
mapc2hs (TString "signed_long_long_int") = TString "CLLong"
mapc2hs (TString "unsigned_long_long_int") = TString "CULLong"
mapc2hs (TString "float") = TString "CFloat"
mapc2hs (TString "double") = TString "CDouble"
mapc2hs (TString "long_double") = TString "CLDouble"
mapc2hs (TString "@@ptrdiff_t@@") = TString "CPtrdiff"
mapc2hs (TString "@@size_t@@") = TString "CSize"
mapc2hs (TString "@@wchar_t@@") = TString "CWchar"
mapc2hs (TString "@@sig_atomic_t@@") = TString "CSigAtomic"
mapc2hs (TString "@@clock_t@@") = TString "CClock"
mapc2hs (TString "@@time_t@@") = TString "CTime"
mapc2hs (TString "@@FILE@@") = TString "CFile"
mapc2hs (TString "@@fpos_t@@") = TString "CFpos"
mapc2hs (TString "@@jmp_buf@@") = TString "CJmpBuf"
mapc2hs (TString "@@void@@") = TString "()"
mapc2hs (TString "void") = TString "()"
mapc2hs (TString "@@variadic@@") = TString "WrongVariadicFunction"

-- Structures, unions (only pointers are valid).

mapc2hs (PtrV n (TString ('s':'t':'r':'u':'c':'t':'_':strname))) = 
  PtrV n (TString ("S_" ++ strname))

mapc2hs (PtrV n (TString ('u':'n':'i':'o':'n':'_':strname))) = 
  PtrV n (TString ("U_" ++ strname))

-- Direct structures/unions: valid in some circumstances, but
-- require special treatment.

mapc2hs (TString ('s':'t':'r':'u':'c':'t':'_':strname)) =
  TString ("@S_" ++ strname)

mapc2hs (TString ('u':'n':'i':'o':'n':'_':strname)) =
  TString ("@U_" ++ strname)

-- Special pseudo-type for bit fields.

mapc2hs (TString ('$':'B':'F':'$':'_':s)) = TString (s ++ "|" ++ ts2ts (mapc2hs (TString s)))

-- Pointer types: map the target type.

mapc2hs (PtrF ps ts) = PtrF ps (mapc2hs ts)
mapc2hs (PtrV ps ts) = PtrV ps (mapc2hs ts)

-- The rest, will be converted into unknown types, and will cause
-- compilation error.

mapc2hs (TString z)  = TString ("Unmapped_C_Type_" ++ z)

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
  isvr (TString "WrongVariadicFunction") = True
  isvr _ = False

-- True if a type is of a function taking/returning direct structures

isdirstruct ts = checktsrec isd ts where
  isd (TString ('@':_)) = True
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
         intern = case (lookupFM tymap ("ID " ++ sym)) of
                  Just (DictId n) -> "___" ++ (show n) ++ "___"
                  other -> ""
         arity (TApply ts) = (length ts) - 1
         arity (PtrF _ t) = arity t
         arity _ = 0
     putStrLn ""
     putStrLn "--"
     putStrLn ""
     case (isv || drs,isf,dyn) of
       (False,True, False) -> do statimport ifn sym tsf
                                 cbckimport sym tsi
       (_,    True, True)  -> skipimport ifn sym
       (False,False,True)  -> do dynimport  ifn sym tsf (arity tsp) intern
                                 cbckimport sym tsi
       (False,False,False) -> varimport  ifn sym tsg True intern
       (True, _,    _)     -> excludeimport (sym ++ " :: " ++ tsg) isv drs
     return ()

-- Exclude an import and explain the reason.

excludeimport sym isv drs = do
  putStrLn   "--"
  putStrLn $ "-- import of function/variable/structure member(s) " ++ sym ++ " is not possible"
  putStrLn $ "-- because of the following reason(s):"
  when isv $ putStrLn "-- function is variadic"
  when drs $ putStrLn "-- function takes/returns structure(s) directly"
  putStrLn   "--"
                  

-- Write an import statement for a function.

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

