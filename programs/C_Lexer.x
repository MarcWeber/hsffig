{

-- This lexer file is based on the following source:
-- http://www.lysator.liu.se/c/ANSI-C-grammar-l.html
-- and converted into Alex syntax from Lex syntax by
-- Dimitry Golubovsky <dimitry@golubovsky.org>, <golubovsky@gmail.com>

-- It is expected that this lexer processes the result of
-- `gcc -E -dD' applied to a C header file, so certain assumptions
-- are made:
--
-- 1. CPP directives like #if(n)def, #if, #endif are absent in the input
-- 2. All #define directives occupy exactly one line

module C_Lexer (
    Token (..),
    AlexPosn(..),
    scanHeader
  ) where
}

%wrapper "posn"

$O  =			[0-7]
$D  =			[0-9]
$L  =			[a-zA-Z_]
$H  =			[a-fA-F0-9]

@E  =			[Ee] [\+\-]? $D+
@FS =			[fFlL]
@IS =			[uUlL]*

tokens :-
"/*"			{ tok (\p s -> TCOMM_OPEN p) }
"*/"			{ tok (\p s -> TCOMM_CLOSE p) }

"auto"			{ tok (\p s -> TKW s p) }
"break"			{ tok (\p s -> TKW s p) }
"case"			{ tok (\p s -> TKW s p) }
"char"			{ tok (\p s -> TKW s p) }
"const"			{ tok (\p s -> TKW s p) }
"__const"		{ tok (\p s -> TKW "const" p) }
"continue"		{ tok (\p s -> TKW s p) }
"default"		{ tok (\p s -> TKW s p) }
"do"			{ tok (\p s -> TKW s p) }
"double"		{ tok (\p s -> TKW s p) }
"else"			{ tok (\p s -> TKW s p) }
"enum"			{ tok (\p s -> TKW s p) }
"extern"		{ tok (\p s -> TKW s p) }
"float"			{ tok (\p s -> TKW s p) }
"for"			{ tok (\p s -> TKW s p) }
"goto"			{ tok (\p s -> TKW s p) }
"if"			{ tok (\p s -> TKW s p) }
"int"			{ tok (\p s -> TKW s p) }
"long"			{ tok (\p s -> TKW s p) }
"register"		{ tok (\p s -> TKW s p) }
"return"		{ tok (\p s -> TKW s p) }
"short"			{ tok (\p s -> TKW s p) }
"signed"		{ tok (\p s -> TKW s p) }
"__signed__"		{ tok (\p s -> TKW "signed" p)}
"inline"		{ tok (\p s -> TKW s p) }
"__inline__"            { tok (\p s -> TKW "inline" p)}
"sizeof"		{ tok (\p s -> TKW s p) }
"static"		{ tok (\p s -> TKW s p) }
"struct"		{ tok (\p s -> TKW s p) }
"switch"		{ tok (\p s -> TKW s p) }
"typedef"		{ tok (\p s -> TKW s p) }
"union"			{ tok (\p s -> TKW s p) }
"unsigned"		{ tok (\p s -> TKW s p) }
"void"			{ tok (\p s -> TKW s p) }
"volatile"		{ tok (\p s -> TKW s p) }
"while"			{ tok (\p s -> TKW s p) }
"__attribute__"		{ tok (\p s -> TKW s p) }
"__extension__"         ;
"#define" $white+ .*	{ tok (\p s -> TKDEF s p) }

$L [$L $D]*		{ tok (\p s -> TKID s p) }

0 [xX] $H+ @IS{0,3}	{ tok (\p s -> TKC_HEX s p) }
0 $D+ @IS{0,3}		{ tok (\p s -> TKC_OCT s p) }
$D+ @IS{0,3}		{ tok (\p s -> TKC_DEC s p) }
L?'(\.|[^\'])+'		{ tok (\p s -> TKC_CHAR s p) }

$D+ @E @FS{0,2}		{ tok (\p s -> TKC_EXP s p) }
$D* \. $D+ @E? @FS{0,2} { tok (\p s -> TKC_EXP s p) }
$D+ \. $D* @E? @FS{0,2}	{ tok (\p s -> TKC_EXP s p) }

L?\"(\.|[^\"])*\"	{ tok (\p s -> TKSTRING s p) }

"..."			{ tok (\p s -> TKOP s p) }
">>="			{ tok (\p s -> TKOP s p) }
"<<="			{ tok (\p s -> TKOP s p) }
"+="			{ tok (\p s -> TKOP s p) }
"-="			{ tok (\p s -> TKOP s p) }
"*="			{ tok (\p s -> TKOP s p) }
"/="			{ tok (\p s -> TKOP s p) }
"%="			{ tok (\p s -> TKOP s p) }
"&="			{ tok (\p s -> TKOP s p) }
"^="			{ tok (\p s -> TKOP s p) }
"|="			{ tok (\p s -> TKOP s p) }
">>"			{ tok (\p s -> TKOP s p) }
"<<"			{ tok (\p s -> TKOP s p) }
"++"			{ tok (\p s -> TKOP s p) }
"--"			{ tok (\p s -> TKOP s p) }
"->"			{ tok (\p s -> TKOP s p) }
"&&"			{ tok (\p s -> TKOP s p) }
"||"			{ tok (\p s -> TKOP s p) }
"<="			{ tok (\p s -> TKOP s p) }
">="			{ tok (\p s -> TKOP s p) }
"=="			{ tok (\p s -> TKOP s p) }
"!="			{ tok (\p s -> TKOP s p) }
";"			{ tok (\p s -> TKOP s p) }
("{"|"<%")		{ tok (\p s -> TKOP "{" p) }
("}"|"%>")		{ tok (\p s -> TKOP "}" p) }
","			{ tok (\p s -> TKOP s p) }
":"			{ tok (\p s -> TKOP s p) }
"="			{ tok (\p s -> TKOP s p) }
"("			{ tok (\p s -> TKOP s p) }
")"			{ tok (\p s -> TKOP s p) }
("["|"<:")		{ tok (\p s -> TKOP "[" p) }
("]"|":>")		{ tok (\p s -> TKOP "]" p) }
"."			{ tok (\p s -> TKOP s p) }
"&"			{ tok (\p s -> TKOP s p) }
"!"			{ tok (\p s -> TKOP s p) }
"~"			{ tok (\p s -> TKOP s p) }
"-"			{ tok (\p s -> TKOP s p) }
"+"			{ tok (\p s -> TKOP s p) }
"*"			{ tok (\p s -> TKOP s p) }
"/"			{ tok (\p s -> TKOP s p) }
"%"			{ tok (\p s -> TKOP s p) }
"<"			{ tok (\p s -> TKOP s p) }
">"			{ tok (\p s -> TKOP s p) }
"^"			{ tok (\p s -> TKOP s p) }
"|"			{ tok (\p s -> TKOP s p) }
"?"			{ tok (\p s -> TKOP s p) }

"#" $white+ .*		{ tok (\p s -> TKFILE s p) }

[ \t\v\n\f]		;
.			;

{

tok f p s = f p s

-- The datatype for Token

data Token = TCOMM_OPEN AlexPosn 
           | TCOMM_CLOSE AlexPosn 
           | TKDEF String AlexPosn
           | TKFILE String AlexPosn
           | TKW String AlexPosn
           | TKOP String AlexPosn
           | TKID String AlexPosn
           | TKC_EXP String AlexPosn
           | TKC_DEC String AlexPosn
           | TKC_OCT String AlexPosn
           | TKC_HEX String AlexPosn
           | TKC_CHAR String AlexPosn
           | TKSTRING String AlexPosn
           deriving (Show)

-- Access to the lexer

scanHeader str = alexScanTokens str
    
-- Token equality. Two tokens are equal if their contents
-- are equal regarding of position.

instance Eq Token where
    (==) (TCOMM_OPEN _) (TCOMM_OPEN _) = True
    (==) (TCOMM_CLOSE _) (TCOMM_CLOSE _) = True
    (==) (TKDEF s1 _) (TKDEF s2 _) = (s1 == s2)
    (==) (TKFILE s1 _) (TKFILE s2 _) = (s1 == s2)
    (==) (TKW s1 _) (TKW s2 _) = (s1 == s2)
    (==) (TKOP s1 _) (TKOP s2 _) = (s1 == s2)
    (==) (TKID s1 _) (TKID s2 _) = (s1 == s2)
    (==) (TKC_EXP s1 _) (TKC_EXP s2 _) = (s1 == s2)
    (==) (TKC_DEC s1 _) (TKC_DEC s2 _) = (s1 == s2)
    (==) (TKC_OCT s1 _) (TKC_OCT s2 _) = (s1 == s2)
    (==) (TKC_HEX s1 _) (TKC_HEX s2 _) = (s1 == s2)
    (==) (TKC_CHAR s1 _) (TKC_CHAR s2 _) = (s1 == s2)
    (==) (TKSTRING s1 _) (TKSTRING s2 _) = (s1 == s2)
    (==) _ _ = False


}


