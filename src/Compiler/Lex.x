{
module Compiler.Lex (tokenise, Token(..)) where
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$symb  = [\+\-\=\/\%\<\>]
tokens :-

  $white+				 ;
  "--".*				 ;
  ":"            { \s -> Colon }
  ";"            { \s -> SemiColon }
  "{"            { \s -> OpenBrac }
  "}"            { \s -> CloseBrac }
  "_"            { \s -> Nop }
  "IF"           { \s -> If }
  "!"            { \s -> Set }
  "@"            { \s -> Deref }
  $alpha+$digit* { Ident }
  $symb{1,2}     { Prim }
  $digit+        { Number . read }

{
data Token = Colon
           | Number Int
           | OpenBrac
           | CloseBrac
           | SemiColon
           | Ident String
           | Prim String
           | If
           | Set
           | Deref
           | Nop
           deriving (Eq,Show)

tokenise = alexScanTokens
}
