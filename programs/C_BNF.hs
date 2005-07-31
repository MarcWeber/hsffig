-- BNF-based C syntax parser (may be incomplete)
-- Based on http://lists.canonical.org/pipermail/kragen-hacks/1999-October/000201.html

-- The C grammar in K&R 2nd Ed is fairly simple, only about 5 pages.
-- Here it is, translated to BNF.  Here ( ) groups, ? means optional, |
-- is alternation, + means one or more, * means zero or more, space means
-- sequence, and "x" means literal x.  As a special abbreviation, x% means 
-- x ("," x)* -- that is, a non-null comma-separated list of x's.

module C_BNF where

import Data.FiniteMap
import Char
import C_Lexer
import Parsec
import ParsecPos
import TokenOps


-- Parse the list of tokens.

data DictElement = DictDecl  Declaration
                 | DictFail
                 | DictDef
                 | DictStruct String [StructDecl]
                 | DictEnum [Enumerator]
                 | DictId Integer

showsemi [] = ""
showsemi (a:as) = (show a) ++ "; " ++ (showsemi as)

showspace [] = ""
showspace (a:as) = (show a) ++ " " ++ (showspace as)

showcomma [] = ""
showcomma [a] = show a
showcomma (a:as) = (show a) ++ ", " ++ (showcomma as)

instance Show DictElement where
  show (DictDecl d) = show d
  show DictFail = "/* DictFail */"
  show DictDef  = "/* DictDef */"
  show (DictStruct su sds) = su ++ " {" ++ (showsemi sds) ++ "}"
  show (DictEnum eds) = "enum {" ++ (showcomma eds) ++ "}"
  show (DictId n) = show n

data PState = PState {
  ucount :: Integer,                       -- unique counter
  tymap  :: FiniteMap String DictElement   -- map for typedefs, structs, etc.
}

type TParser a = GenParser Token PState a

afm rn di st = PState {
  ucount = (ucount st) + 1,
  tymap = addToFM (tymap st) rn di
}

upducnt st = PState {
  ucount = (ucount st) + 1,
  tymap = tymap st
}

lookmap st = lookupFM (tymap st)

getucnt st = do
  updateState upducnt
  st <- getState
  return $ ucount st

maybeOpt p =
  try (do t <- p
          return (Just t))
  <|>
  return Nothing

bnfParser = do tu <- translation_unit
               st <- getState
               return (tu, tymap st)


-- Record a structure. Structure information is saved
-- in the same map where type aliases are kept.

recordstruct s i sds = do
  let recname = s ++ "@" ++ i
      dcs = DictStruct s sds
  case sds of
    [] -> do st <- getState
             case (lookmap st recname) of
               Nothing -> updateState (afm recname dcs)
               (Just _) -> return ()
    other -> updateState (afm recname dcs)

-- Record an enum. Enum information is saved
-- in the same map where type aliases are kept.

recordenum i eds = do
  let recname = "enum@" ++ i
  case eds of
    [] -> do st <- getState
             case (lookmap st recname) of
               Nothing -> updateState (afm recname (DictEnum eds))
               (Just _) -> return ()
    other -> updateState (afm recname (DictEnum eds))


-- To resolve the typedef problem, typedefs are made into
-- translation units. Each typedef updates parser's internal
-- state with the pair "key-value" where value is type expression
-- and key is type alias. If a potential typedef-name is encountered,
-- it is checked against the map, and, if found, parse is successful
-- otherwise it fails.

-- For preprocessor defines, we really need to keep only name: later,
-- when generating Haskell code (hsc), each constant will be given accessor
-- function. Whether the constant is parameterized, or contains anything
-- else than just a literal, does not matter: illegal usage will be detected
-- at compile time.

-- translation-unit: (typedef-declaration | function-definition | declaration | ignored)+

data TransUnit = TransUnitFun  FunDef
               | TransUnitDecl Declaration
               | TypedefDecl   Declaration String
               | TransUnitDef  String
               | Ignored       String
               deriving (Show)

translation_unit = many1 translation_unit'

translation_unit' = 
  try (do t <- typedef_declaration
          return t) 
  <|>
  try (do f <- function_definition
          return (TransUnitFun f))
  <|>
  try (do d <- declaration
          return (TransUnitDecl d))
  <|>
  try (do t <- anyToken
          case t of
                 (TKDEF s p) -> do let c = ppdef2name s
                                   updateState (afm c DictDef)
                                   return (TransUnitDef c)
                 _ -> return (Ignored (showTok t)))
  <?> "translation_unit"

ppdefchar c
  | (isAlphaNum c) = True
  | (c == '_') = True
  | otherwise = False

sndword [] = ""
sndword (w:[]) = ""
sndword (fs:sn:wds) = sn

ppdef2name s = nf (sndword $ words s)
  where nf = takeWhile ppdefchar


-- Retrieve a type alias name from a typedef-related Declaration
-- and modify the Declaration to declare a type.

-- The Declaration data structure contains a list of declaration
-- specifiers, and a list (must be singleton) of initialization
-- declarators. The list of declaration specifiers remains untouched.
-- The initialization declarator (InitDeclarator) contains a Declarator
-- which in turn must contain a Left String where String contains
-- the name of the type alias. For function pointer types, InitDeclarator
-- contains a Right Declarator (nested Right Declarators), 
-- last of them containing Left String. The function returns a tuple
-- containing the modified Declaration (InitDeclarator's Left String
-- is set to empty string), and the type alias retrieved from
-- InitDeclarator.

decl2str :: Declaration -> (Declaration,String)

decl2str d@(Declaration ds [] _) = error $ "decl2str: no InitDeclarators\n" ++ (show d)
decl2str (Declaration ds idds at) 
  | (length idds) > 1 = error ("decl2str: multiple InitDeclarators: " ++ show idds)
  | otherwise = (decmod,tystr) where
    decmod = Declaration ds [hidd'] at
    hidd = head idds
    tystr = id2str hidd
    hidd' = modidd hidd
    id2str (InitDeclarator d i) = d2str d
    d2str (Declarator _ (Right dd) _) = d2str dd
    d2str (Declarator _ (Left str) _) = str
    modidd (InitDeclarator d i) = InitDeclarator (modd d) i
    modd (Declarator ps (Right dd) cpis) = Declarator ps (Right (modd dd)) cpis
    modd (Declarator ps (Left str) cpis) = Declarator ps (Left "") cpis


-- Fix anonymous structures/unions/whatever in typedefs.
-- Converted name of the type alias is substituted as structure/whatever
-- name to eliminate confusion with anonymous structures. Such a confusion
-- happens later at the FFI imports generation stage, when typedefs
-- are replaced with their original types, which may result in
-- "type name disappearance".

anonfix s (Declaration dss ids ats) = 
  Declaration (map (anonfix' s') dss) (map (anonfix'' s'') ids) ats
  where anonfix' s (DeclSpecType (TypeSpecStruct (StructSpec so "" sds))) = 
          DeclSpecType (TypeSpecStruct (StructSpec so s sds))
        anonfix' s (DeclSpecType (TypeSpecEnum (EnumSpec "" es))) = 
          DeclSpecType (TypeSpecEnum (EnumSpec s es))
        anonfix' s z = z
        anonfix'' s z = z
        s' = s ++ "'"
        s'' = s ++ "_p"

-- typedef-declaration:
--   "typedef" declaration 

-- Per FFI Addendum, types ptrdiff_t and further down the following list
-- have specific mapping to Haskell types. these are not "primitive types"
-- per se.
-- If a typedef statement is met with one of those special types,
-- it is converted into a typedef declaration with original primitive
-- type same as the type alias name, but enclosed in @@'s. Later,
-- these types will be converted according to the Addendum.

primspeclist = ["ptrdiff_t", "size_t", "wchar_t", "sig_atomic_t",
                "clock_t", "time_t",
                "FILE", "fpos_t", "jmp_buf"]

unrolldecl (Declaration ds [] at) = []
unrolldecl (Declaration ds (i:is) at) = (Declaration ds [i] at) : 
                                        (unrolldecl (Declaration ds is at))

recordtdef d = do
  let (dcmod, tystr) = decl2str d
      fixdecl = if (tystr `elem` primspeclist) then selfprim else d
      selfprim = Declaration [DeclSpecType (TypeSpecPrim ("@@" ++ tystr ++ "@@"))]
                             [InitDeclarator (Declarator [] (Left "") []) Nothing]
                             []
  updateState (afm tystr (DictDecl fixdecl))
  return (TypedefDecl fixdecl tystr)

typedef_declaration = 
  try (do tkKw "typedef"
          d <- declaration
          ts <- mapM recordtdef $ unrolldecl d
          return (head ts))
  <?> "typedef_declaration"

-- function-definition: 
--   declaration-specifiers? declarator declaration* block

data FunDef = FunDef [DeclSpec] Declarator [Declaration] Block
            deriving (Show)

function_definition = 
  try (do ds <- declaration_specifiers
          dd <- declarator
          dn <- many declaration
          bk <- block
          return (FunDef ds dd dn bk))
  <|>
  try (do dd <- declarator
          dn <- many declaration
          bk <- block
          return (FunDef [] dd dn bk))
  <?> "function_definition"

-- declaration: declaration-specifiers gnuc_attribute? init-declarator%  gnuc_attribute? ";"

data Declaration = Declaration [DeclSpec] [InitDeclarator] [Attribute]
  
instance Show Declaration where
  show (Declaration ds ids _) = (showspace ds) ++ " " ++ (showcomma ids) ++ ";"

declaration = 
  try (do at1 <- gnuc_attribute
          ds <- declaration_specifiers
          id <- init_declarator `sepBy` (tkOp ",")
          at2 <- gnuc_attribute
          tkOp ";"
          return (Declaration ds id (at1 ++ at2)))
  <?> "declaration"

-- For now, just recognize the outermost attribute parens.
-- Make no distinction on inner tokens (they most likely will be identifiers)

-- gnuc_attribute: "__attribute__" "(" "(" attr% ")" ")
-- attr: token ("(" token% ")")?

data Attribute = Attribute String [String]
               deriving (Show)

gnuc_attribute = 
  try (do tkKw "__attribute__"
          tkOp "("
          tkOp "("
          as <- attr `sepBy1` (tkOp ",")
          tkOp ")"
          tkOp ")"
          return as)
  <|> (return [])

attr = 
  try (do an <- anyToken
          tkOp "("
          av <- anyToken `sepBy1` (tkOp ",")
          tkOp ")"
          return (Attribute (showTok an) (map showTok av)))
  <|>
  try (do an <- anyToken
          return (Attribute (showTok an) []))
  <?> "attr"


-- declaration-specifiers: 
--   (storage-class-specifier | type-specifier | type-qualifier)+

data DeclSpec = DeclSpecStor  String
              | DeclSpecType  TypeSpec
              | DeclSpecTqual String

instance Show DeclSpec where
  show (DeclSpecStor s) = s
  show (DeclSpecType t) = show t
  show (DeclSpecTqual s) = s

declaration_specifiers = many1 declaration_specifier

declaration_specifier =
  try (do s <- storage_class_specifier
          return (DeclSpecStor s))
  <|>
  try (do t <- type_specifier
          return (DeclSpecType t))
  <|>
  try (do q <- type_qualifier
          return (DeclSpecTqual q))
  <?> "declaration_specifier"

-- typedef is removed from storage class specifiers.

-- storage-class-specifier: 
--   ("auto" | "register" | "static" | "extern" | "inline")

storage_class_specifier = 
  (foldr1 (<|>) 
          (map (try . kwString) 
               ["auto", "register", "static", "extern","inline"]))
  <?> "storage_class_specifier"

-- type-specifier: ("void" | "char" | "short" | "int" | "long" | "float" |
--   "double" | "signed" | "unsigned" | struct-or-union-specifier | 
--   enum-specifier | typedef-name)

data TypeSpec = TypeSpecPrim   String
              | TypeSpecStruct StructSpec
              | TypeSpecEnum   EnumSpec
              | TypeSpecAlias  String

instance Show TypeSpec where
  show (TypeSpecPrim s) = s
  show (TypeSpecStruct s) = show s
  show (TypeSpecEnum e) = show e
  show (TypeSpecAlias s) = s

type_specifier = 
  try (do s <- type_spec_prim
          return (TypeSpecPrim s))
  <|>
  try (do s <- struct_or_union_specifier
          return (TypeSpecStruct s))
  <|>
  try (do e <- enum_specifier
          return (TypeSpecEnum e))
  <|>
  try (do t <- typedef_name
          return (TypeSpecAlias t))
  <?> "type_specifier"


primtypelist = ["void", "char", "short", "int",
                "long", "float", "double",
                "signed", "unsigned"]

type_spec_prim = 
  (foldr1 (<|>)
          (map (try . kwString) primtypelist))
  <?> "type_spec_prim"

-- type-qualifier: ("const" | "volatile")

type_qualifier =
  (foldr1 (<|>)
          (map (try . kwString)
               ["const", "volatile"]))
  <?> "storage_class_specifier"

-- Parser state contains a finite map of type aliases.
-- The candidate is looked up in the map. The parser
-- succeeds if the token is found and fails otherwise.

typedef_name = do tc <- anyIdString
                  st <- getState
                  case (lookmap st tc) of
                    (Just (DictDecl _)) -> return tc
                    _                   -> (do updateState (afm tc DictFail)
                                               fail "typedef_name")
              


-- struct-or-union-specifier: 
--   ("struct" | "union") (
--     identifier? "{" struct-declaration+ "}" |
--     identifier
--   )

data StructSpec = StructSpec String String [StructDecl]

instance Show StructSpec where
  show (StructSpec s i sds) = s ++ " " ++ (showanon i) ++ (shownonempty sds)  where
    showanon x = case (isDigit $ head x) of
                   True -> ""
                   False -> x
    shownonempty [] = ""
    shownonempty z = "{" ++ (showsemi z) ++ "}"

struct_or_union_specifier = 
  try (do s <- struct_union
          i <- anyIdString
          tkOp "{"
          ds <- many struct_declaration
          tkOp "}"
          recordstruct s i ds
          return (StructSpec s i ds))
  <|>
  try (do let emuid st = show $ ucount st
          s <- struct_union
          tkOp "{"
          ds <- many struct_declaration
          tkOp "}"
          ct <- getState >>= getucnt
          recordstruct s (show ct) ds
          return (StructSpec s (show ct) ds))
  <|>
  try (do s <- struct_union
          i <- anyIdString
          recordstruct s i []
          return (StructSpec s i []))
  <?> "struct_or_union_specifier"

struct_union = 
  (foldr1 (<|>)
          (map (try . kwString)
               ["struct", "union"]))
  <?> "struct_union"

-- init-declarator: declarator ("=" initializer)?

data InitDeclarator = InitDeclarator Declarator (Maybe Initializer)

instance Show InitDeclarator where
  show (InitDeclarator d mbi) = (show d) ++ (mbshow mbi) where
    mbshow Nothing = ""
    mbshow (Just i) = " = " ++ (show i)

init_declarator = 
  try (do d <- declarator
          i <- maybeOpt (do tkOp "="
                            ini <- initializer
                            return ini)
          return (InitDeclarator d i))
  <?> "init_declarator"

-- struct-declaration: 
--   (type-specifier | type-qualifier)+ struct-declarator% ";"

tstq = 
  try (do t <- type_specifier
          return (DeclSpecType t))
  <|>
  try (do q <- type_qualifier
          return (DeclSpecTqual q))
  <?> "tstq"

data StructDecl = StructDecl [DeclSpec] [StructDeclarator]

instance Show StructDecl where
  show (StructDecl dss sds) = (showspace dss) ++ " " ++ showcomma sds

struct_declaration = 
  try (do tqs <- many1 tstq
          sds <- struct_declarator `sepBy1` (tkOp ",")
          tkOp ";"
          return (StructDecl tqs sds))
  <?> "struct_declaration"

-- struct-declarator: declarator | declarator? ":" constant-expression

data StructDeclarator = StructDeclarator (Maybe Declarator) (Maybe ConstExpr)

instance Show StructDeclarator where
  show (StructDeclarator mbd mbc) = (smbd mbd) ++ (smbc mbc) where
    smbd (Nothing) = ""
    smbd (Just d) = show d
    smbc (Nothing) = ""
    smbc (Just c) = ":" ++ (show c)

struct_declarator = 
  try (do d <- maybeOpt declarator
          c <- maybeOpt (do tkOp ":"
                            ce <- constant_expression
                            return ce)
          return (StructDeclarator d c))
  <?> "struct_declarator"

-- enum-specifier: "enum" (identifier | identifier? "{" enumerator% "}")

data EnumSpec = EnumSpec String [Enumerator]
              deriving (Show)

enum_specifier = 
  try (do tkKw "enum"
          i <- anyIdString
          tkOp "{"
          e <- enumerators
          tkOp "}"
          recordenum i e
          return (EnumSpec i e))
  <|>
  try (do tkKw "enum"
          tkOp "{"
          e <- enumerators
          tkOp "}"
          ct <- getState >>= getucnt
          recordenum (show ct) e
          return (EnumSpec (show ct) e))
  <|>
  try (do tkKw "enum"
          i <- anyIdString
          recordenum i []
          return (EnumSpec i []))
  <?> "enum_specifier"

enumerators = 
  try (do e <- many1 (do en <- enumerator
                         tkOp ","
                         return en)
          return e)
  <|>
  try (do e <- enumerator `sepBy1` (tkOp ",")
          return e)
  <?> "enumerators"

-- enumerator: identifier ("=" constant-expression)?

data Enumerator = Enumerator String (Maybe ConstExpr)

instance Show Enumerator where
  show (Enumerator s mbce) = s ++ (showmbce mbce) where
    showmbce Nothing = ""
    showmbce (Just ce) = " = " ++ (show ce)

enumerator = 
  try (do i <- anyIdString
          c <- maybeOpt (do tkOp "="
                            ce <- constant_expression
                            return ce)
          return (Enumerator i c))
  <?> "enumerator"

  
-- declarator: 
--   pointer? (identifier | "(" declarator ")") (
--     "[" constant-expression? "]" |
--     "(" parameter-type-list ")" |
--     "(" identifier%? ")"
--   )*

data Declarator = Declarator [Pointer] (Either String Declarator) [CPI]

data CPI = CPICon (Maybe ConstExpr)
         | CPIPar [ParamDecl] Variadic
         | CPIId  [String]
         | CPIEmpty

instance Show Declarator where
  show (Declarator ps esd cpis) = sps ++ (sesd esd) ++ (scpi cpis) where
    sps = take (length ps) ['*', '*' ..]
    sesd (Left s) = s
    sesd (Right d) = "(" ++ (show d) ++ ")"
    scpi [] = ""
    scpi cs = showspace cs

instance Show CPI where
  show (CPICon Nothing) = "[]"
  show (CPICon (Just c)) = "[" ++ (show c) ++ "]"
  show (CPIPar ps v) = "(" ++ (showcomma ps) ++ (dots v) ++ ")" where
    dots Fixed = ""
    dots Variadic = ", ..."
  show (CPIId ss) = "(" ++ (showcomma ss) ++ ")"
  show CPIEmpty = ""

-- All identifiers are recorded in the parser state map with unique
-- numbers (size of the map at the moment of identifier occurrence).
-- This helps further on to generate internal names for imported variables
-- and functions.

recordid s = do
  st <- getState
  let key = "ID " ++ s
  case (lookmap st key) of 
                         Just _ -> return ()
                         Nothing -> updateState (afm key (DictId (ucount st)))

declarator = 
  try (do ps <- pointer
          id <- idd
          cp <- many cpi
          return (Declarator ps id cp))
  <?> "declarator"

idd = 
  try (do s <- anyIdString
          recordid s
          return (Left s))
  <|>
  try (do tkOp "("
          d <- declarator
          tkOp ")"
          return (Right d))
  <?> "idd"


cpi = 
  try (do tkOp "["
          c <- maybeOpt constant_expression
          tkOp "]"
          return (CPICon c))
  <|>
  try (do tkOp "("
          (p,v) <- parameter_type_list
          tkOp ")"
          case p of
            [] -> return CPIEmpty
            other -> return (CPIPar p v))
  <|>
  try (do tkOp "("
          i <- anyIdString `sepBy1` (tkOp ",")
          tkOp ")"
          return (CPIId i))
  <|>
  try (do tkOp "("
          tkOp ")"
          return CPIEmpty)
  <?> "cpi"

-- pointer:
--   ("*" type-qualifier*)*

data Pointer = Pointer [String]
             deriving (Show)

pointer = many pointer'

pointer' = 
  try (do tkOp "*"
          q <- many type_qualifier
          return (Pointer q))
  <?> "pointer"

-- parameter-type-list: "void" | parameter-declaration% ("," "...")?

data Variadic = Variadic
              | Fixed
              deriving (Show)

parameter_type_list = 
  try (do pd <- many1 (do p <- parameter_declaration 
                          tkOp ","
                          return p)
          tkOp "..."
          return (pd,Variadic))
  <|>
  try (do pd <- parameter_declaration `sepBy1` (tkOp ",")
          return (pd,Fixed))
  <|>
  try (do tkKw "void"
          return ([],Fixed))
  <?> "parameter_type_list"

-- parameter-declaration: 
--   declaration-specifiers (declarator | abstract-declarator)?

data ParamDecl = ParamDecl [DeclSpec] (Maybe Declarator)

instance Show ParamDecl where
  show (ParamDecl ds mbd) = (showspace ds) ++ " " ++ (mbshow mbd) where
    mbshow Nothing = ""
    mbshow (Just d) = show d

parameter_declaration = 
  try (do ds <- declaration_specifiers
          dd <- maybeOpt dad 
          return (ParamDecl ds dd))
  <?> "parameter_declaration"

dad = 
  try (do d <- declarator
          return d)
  <|>
  try (do d <- abstract_declarator
          return d)
  <?> "dad"

-- initializer: assignment-expression | "{" initializer% ","? "}"

data Initializer = InitializerAsg  AssignmentExpr
                 | InitializerInit [Initializer] Bool
                 deriving (Show)

initializer = 
  try (do a <- assignment_expression
          return (InitializerAsg a))
  <|>
  try (do tkOp "{"
          i <- initializer `sepBy1` (tkOp ",")
          tkOp ","
          tkOp "}"
          return (InitializerInit i True))
  <|>
  try (do tkOp "{"
          i <- initializer `sepBy1` (tkOp ",")
          tkOp "}"
          return (InitializerInit i False))
  <?> "initializer"

-- type-name: (type-specifier | type-qualifier)+ abstract-declarator?

data TypeName = TypeName [DeclSpec] (Maybe Declarator)

instance Show TypeName where
  show (TypeName ds mbd) = (showspace ds) ++ " " ++ (mbshow mbd) where
    mbshow Nothing = ""
    mbshow (Just d) = show d


type_name = 
  try (do tq <- many1 tstq
          ad <- maybeOpt abstract_declarator
          return (TypeName tq ad))
  <?> "type_name"

-- abstract-declarator: 
--   pointer ("(" abstract-declarator ")")? (
--     "[" constant-expression? "]" |
--     "(" parameter-type-list? ")"
--   )*

abstract_declarator = 
  try (do ps <- pointer
          ad <- maybeOpt (do tkOp "("
                             ada <- abstract_declarator
                             tkOp ")"
                             return ada)
          cp <- many cpi
          let dd = case ad of
                     (Just x) -> Right x
                     Nothing  -> Left ""
          return (Declarator ps dd cp))
  <?> "abstract_declarator"

-- statement:
--   ((identifier | "case" constant-expression | "default") ":")*
--   (expression? ";" | 
--    block | 
--    "if" "(" expression ")" statement |
--    "if" "(" expression ")" statement "else" statement |
--    "switch" "(" expression ")" statement |
--    "while" "(" expression ")" statement |
--    "do" statement "while" "(" expression ")" ";" |
--    "for" "(" expression? ";" expression? ";" expression? ")" statement |
--    "goto" identifier ";" |
--    "continue" ";" |
--    "break" ";" |
--    "return" expression? ";"
--   )

data Statement = Statement [StatLabel] GenStat
               deriving (Show)

data StatLabel = StatLabelId String
               | StatLabelCase ConstExpr
               | StatLabelDefault
               deriving (Show)

data GenStat = GenStatExpr Expr
             | GenStatEmpty
             | GenStatBlock Block
             | GenStatIf Expr Statement
             | GenStatIfElse Expr Statement Statement
             | GenStatSwitch Expr Statement
             | GenStatWhile Expr Statement
             | GenStatDo Expr Statement
             | GenStatFor (Maybe Expr) (Maybe Expr) (Maybe Expr) Statement
             | GenStatGoto String
             | GenStatContinue
             | GenStatBreak
             | GenStatReturn (Maybe Expr)
             deriving (Show)

statement = 
  try (do st <- many stat_label
          gs <- gen_statement
          return (Statement st gs))
  <?> "statement"

stat_label = 
  try (do i <- anyIdString
          tkOp ":"
          return (StatLabelId i))
  <|>
  try (do tkKw "case"
          c <- constant_expression
          tkOp ":"
          return (StatLabelCase c))
  <|>
  try (do tkKw "default"
          tkOp ":"
          return StatLabelDefault)
  <?> "stat_label"

else_stat = 
  try (do tkKw "else"
          s <- statement
          return s)
  <?> "else_stat"

gen_statement = 
  try (do e <- maybeOpt expr
          tkOp ";"
          return (case e of
                       Just e  -> (GenStatExpr e)
                       Nothing -> GenStatEmpty))
  <|>
  try (do b <- block
          return (GenStatBlock b))
  <|>
  try (do tkKw "if"
          tkOp "("
          e <- expr
          tkOp ")"
          si <- statement
          se <- else_stat
          return (GenStatIfElse e si se))
  <|>
  try (do tkKw "if"
          tkOp "("
          e <- expr
          tkOp ")"
          si <- statement
          return (GenStatIf e si))
  <|>
  try (do tkKw "switch"
          tkOp "("
          e <- expr
          tkOp ")"
          s <- statement
          return (GenStatSwitch e s))
  <|>
  try (do tkKw "while"
          tkOp "("
          e <- expr
          tkOp ")"
          s <- statement
          return (GenStatWhile e s))
  <|>     
  try (do tkKw "do"
          s <- statement
          tkKw "while"
          tkOp "("
          e <- expr
          tkOp ")"
          tkOp ";"
          return (GenStatDo e s))
  <|>
  try (do tkKw "for"
          tkOp "("
          e1 <- maybeOpt expr
          tkOp ";"
          e2 <- maybeOpt expr
          tkOp ";"
          e3 <- maybeOpt expr
          tkOp ")"
          s <- statement
          return (GenStatFor e1 e2 e3 s))
  <|>
  try (do tkKw "goto"
          i <- anyIdString
          tkOp ";"
          return (GenStatGoto i))
  <|>
  try (do tkKw "continue"
          tkOp ";"
          return GenStatContinue)
  <|>
  try (do tkKw "break"
          tkOp ";"
          return GenStatBreak)
  <|> 
  try (do tkKw "return"
          e <- maybeOpt expr
          tkOp ";"
          return (GenStatReturn e))
  <?> "gen_statement"

-- block: "{" declaration* statement* "}"

data Block = Block [Declaration] [Statement]
           deriving (Show)

block = 
  try (do tkOp "{"
          ds <- many declaration
          ss <- many statement
          tkOp "}"
          return (Block ds ss))
  <?> "block"

-- expression: 
--   assignment-expression%

type Expr = [AssignmentExpr]

expr = 
  try (do as <- assignment_expression `sepBy1` (tkOp ",")
          return as)
  <?> "expr"

-- Expressions are represented as application of a binary operation
-- to operands.

-- assignment-expression: (
--     unary-expression (
--       "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" |
--       "^=" | "|="
--     )
--   )* conditional-expression

data AssignmentExpr = AssignmentExpr [(BinExpr,String)] CondExpr

instance Show AssignmentExpr where
  show (AssignmentExpr ue ce) = (concat $ map showue ue) ++ " " ++ (show ce) where
    showue (u,o) = (show u) ++ " " ++ o

assignment_expression = 
  try (do ue <- many unary_assign
          ce <- conditional_expression
          return (AssignmentExpr ue ce))
  <?> "assignment_expression"

unary_assign = 
  try (do u <- unary_expression
          o <- assignment_operation
          return (u,o))
  <?> "unary_assign"

assignment_operation = 
  (foldr1 (<|>)
          (map (try . opString)
               ["=", "*=", "/=", "%=", "+=",
                "+=", "-=", "<<=", ">>=",
                "&=", "^=", "|="]))
  <?> "assignment_operation"

-- constant-expression: conditional-expression

type ConstExpr = CondExpr

constant_expression = conditional_expression

-- conditional-expression:
--   logical-OR-expression ( "?" expression ":" conditional-expression )?

data CondExpr = CondExpr BinExpr (Maybe (Expr, CondExpr))

instance Show CondExpr where
  show (CondExpr le qm) = (show le) ++ (mbshow qm) where
    mbshow Nothing = ""
    mbshow (Just (e,c)) = "?" ++ (show e) ++ ":" ++ (show c)

conditional_expression = 
  try (do le <- logical_OR_expression
          qm <- maybeOpt (do tkOp "?"
                             e <- expr
                             tkOp ":"
                             c <- conditional_expression
                             return (e,c))
          return (CondExpr (liftExpr le) qm))
  <?> "conditional_expression"

-- A common case for a binary operation: a recursive type;
-- recursion ends when a cast-expression occurs.

data BinExpr = BinExpr BinExpr [(String,BinExpr)]
             | CastExpr [TypeName] BinExpr
             | UnExpr [String] UnExprGen

instance Show BinExpr where
  show (BinExpr l sr) = "(" ++ (show l) ++ ")" ++ (concat $ map showsr sr) where
    showsr (s,r) = s ++ " " ++ (show r)
  show (CastExpr tns b) = (concat $ map showtns tns) ++ "(" ++ (show b) ++ ")" where
    showtns s = "(" ++ (show s) ++ ")"
  show (UnExpr ss ug) = (concat $ map (" " ++)  ss) ++ " " ++ (show ug)
  

binExpr sub op failid =
  try (do l <- sub
          r <- try $ many (do o <- op
                              e <- sub
                              return (o,e))
          return (BinExpr l r))
  <?> failid

-- A binary expression with empty list of operation-expression pairs
-- may be reduced.

liftExpr (CastExpr ts u)
  | (length ts) == 0 = liftExpr u
  | otherwise = CastExpr ts u
liftExpr (BinExpr b ps)
  | (length ps) == 0 = liftExpr b
  | otherwise = BinExpr b ps
liftExpr z = z

-- logical-OR-expression:
--   logical-AND-expression ( "||" logical-AND-expression )*

logical_OR_expression = binExpr logical_AND_expression (opString "||") "logical_OR_expression"

-- logical-AND-expression:
--   inclusive-OR-expression ( "&&" inclusive-OR-expression )*

logical_AND_expression = binExpr inclusive_OR_expression (opString "&&") "logical_AND_expression"

-- inclusive-OR-expression:
--   exclusive-OR-expression ( "|" exclusive-OR-expression )*

inclusive_OR_expression = binExpr exclusive_OR_expression (opString "|") "inclusive_OR_expression"


-- exclusive-OR-expression:
--   AND-expression ( "^" AND-expression )*

exclusive_OR_expression = binExpr aND_expression (opString "^") "exclusive_OR_expression"


-- AND-expression:
--   equality-expression ( "&" equality-expression )*

aND_expression = binExpr equality_expression (opString "&") "aND_expression"


-- equality-expression:
--   relational-expression ( ("==" | "!=") relational-expression )*

equality_expression = binExpr relational_expression equality_operation "equality_expression"


equality_operation =
  (foldr1 (<|>)
          (map (try . opString)
               ["==", "!="]))
  <?> "equality_operation"

-- relational-expression:
--   shift-expression ( ("<" | ">" | "<=" | ">=") shift-expression )*

relational_expression = binExpr shift_expression relational_operation "relational_expression"

relational_operation =
  (foldr1 (<|>)
          (map (try . opString)
               ["<=", ">=", "<", ">"]))
  <?> "relational_operation"

-- shift-expression:
--   additive-expression ( ("<<" | ">>") additive-expression )*

shift_expression = binExpr additive_expression shift_operation "shift_expression"

shift_operation = 
  (foldr1 (<|>)
          (map (try . opString)
               ["<<", ">>"]))
  <?> "shift_operation"

-- additive-expression:
--   multiplicative-expression ( ("+" | "-") multiplicative-expression )*

additive_expression = binExpr multiplicative_expression additive_operation "additive_expression"

additive_operation =
  (foldr1 (<|>)
          (map (try . opString)
               ["+", "-"]))
  <?> "additive_operation"

-- multiplicative-expression:
--   cast-expression ( ("*" | "/" | "%") cast-expression )*

multiplicative_expression = binExpr cast_expression multiplicative_operation 
                                    "multiplicative_expression"

multiplicative_operation =
  (foldr1 (<|>)
          (map (try . opString)
               ["*", "/", "%"]))
  <?> "multiplicative_operation"

-- cast-expression:
--   ( "(" type-name ")" )* unary-expression

cast_expression = 
  try (do ts <- many paren_typename 
          ue <- unary_expression
          return (CastExpr ts ue))
  <?> "cast_expression"

paren_typename = 
  try (do tkOp "("
          tn <- type_name
          tkOp ")"
          return tn)


-- unary-expression:
--   ("++" | "--" | "sizeof" ) * ( 
--     "(" type-name ")"                                    |
--     ("&" | "*" | "+" | "-" | "~" | "!" ) cast-expression |
--     postfix-expression
--   )

data UnExprGen = UnExprSizeof TypeName
               | UnExprCast   String BinExpr
               | UnExprPfx    PostfixExpr

instance Show UnExprGen where
  show (UnExprSizeof tn) = "(" ++ (show tn) ++ ")"
  show (UnExprCast s b) = "(" ++ s ++ (show b) ++ ")"
  show (UnExprPfx p) = show p

unary_expression = 
  try (do s <- many unary_operation
          g <- unary_expr_gen
          return (UnExpr s g))
  <?> "unary_expression"

unary_operation = 
  try (opString "++")
  <|>
  try (opString "--")
  <|>
  try (kwString "sizeof")
  <?> "unary_operation"

unary_cast_operation =
  (foldr1 (<|>)
          (map (try . opString)
               ["&", "*", "+", "-", "~", "!"]))
  <?> "unary_cast_operation"

unary_expr_gen = 
  try (do t <- paren_typename
          return (UnExprSizeof t))
  <|>
  try (do s <- unary_cast_operation
          c <- cast_expression
          return (UnExprCast s c))
  <|>
  try (do p <- postfix_expression
          return (UnExprPfx p))
  <?> "unary_expr_gen"

-- postfix-expression:
--   (identifier | constant | string | "(" expression ")") (
--     "[" expression "]"             |
--     "(" assignment-expression% ")" |
--     "." identifier                 |
--     "->" identifier                |
--     "++"                           |
--     "--"
--   )*

data PostfixExpr = PostfixExpr PfxLeft [PfxRight]

instance Show PostfixExpr where
  show (PostfixExpr l r) = (show l) ++ " " ++ (showspace r)

data PfxLeft = PfxId     String
             | PfxConst  String
             | PfxString String
             | PfxExpr   Expr

instance Show PfxLeft where
  show (PfxId s) = s
  show (PfxConst s) = s
  show (PfxString s) = show s
  show (PfxExpr e) = showspace e

data PfxRight = PfxSquare Expr
              | PfxParens Expr
              | PfxRef    String String
              | PfxOp     String

instance Show PfxRight where
  show (PfxSquare e) = "[" ++ (show e) ++ "]"
  show (PfxParens e) = "(" ++ (show e) ++ ")"
  show (PfxRef s1 s2) = s1 ++ s2
  show (PfxOp s) = s

postfix_expression = 
  try (do l <- postfix_left
          r <- many postfix_right
          return (PostfixExpr l r))
  <?> "postfix_expression"

postfix_left = 
  try (do s <- anyIdString
          return (PfxId s))
  <|>
  try (do c <- anyConst
          return (PfxConst (showTok c)))
  <|>
  try (do s <- anyString
          return (PfxString (showTok s))) 
  <|>
  try (do tkOp "("
          e <- expr
          tkOp ")"
          return (PfxExpr e))
  <?> "postfix_left"

postfix_right = 
  try (do tkOp "["
          e <- expr
          tkOp "]"
          return (PfxSquare e))
  <|>
  try (do tkOp "("
          e <- expr
          tkOp ")"
          return (PfxParens e))
  <|>
  try (do r <- reference_operation
          i <- anyIdString
          return (PfxRef i r))
  <|>
  try (do p <- postfix_operation
          return (PfxOp p))
  <?> "postfix_right"

reference_operation = 
  try (opString ".")                 
  <|>  
  try (opString "->")
  <?> "reference_operation"

postfix_operation = 
  try (opString "++")                 
  <|>  
  try (opString "--")
  <?> "postfix_operation"

