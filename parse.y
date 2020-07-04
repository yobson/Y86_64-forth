{
module Parse (buildExpr, Expr(..), foldExpr) where

import qualified Lex as L
}

%name buildExpr
%tokentype { Tokens }
%error { parseError }

%token
      num   { L.Number $$ }
      '{'   { L.OpenBrac }
      '}'   { L.CloseBrac }
      ':'   { L.Colon }
      ';'   { L.SemiColon }
      id    { L.Ident $$ }
      prim  { L.Prim $$ }
      if    { L.If }
      nop   { L.Nop }
      '!'   { L.Set }
      '@'   { L.Deref }

%%

Exp  : SExp Exp                    { $1 :> $2 }
     | SExp                        { $1 }

SExp : num                          { Number $1 }
     | id                           { Variable $1 }
     | prim                         { Prim $1 }
     | nop                          { Nop }
     | id '!'                       { Set $1 }
     | id '@'                       { Deref $1 }
     | if '{' Exp '}' '{' Exp '}'   { If $3 $6 }
     | ':' id Exp ';'               { Closure $2 [] $3 }
     | ':'  id '{' List '}' Exp ';' { Closure $2 $4 $6 }

List : id                           { [$1] }
     | id List                      { $1:$2 }

{
type Tokens = L.Token
parseError :: [Tokens] -> a
parseError xs = error ("Parse error" ++ show xs)

type Ident = String
type Stack = [Int]
type Env   = [(Ident, Expr)]

data Expr = Number Int 
          | Variable Ident 
          | Closure Ident [Ident] Expr
          | Expr :> Expr
          | Prim Ident
          | If Expr Expr
          | CodePop Int -- For codeGen
          | Nop
          | Deref Ident
          | Set Ident
          deriving (Show)

foldExpr :: (Int -> b) -> (Ident -> b) -> (Ident -> [Ident] -> b -> b) -> (b -> b -> b)
         -> (Ident -> b) -> (b -> b -> b) -> b -> (Int -> b) -> (Ident -> b)
         -> (Ident -> b) -> Expr -> b
foldExpr n v c j p f o cp d s (Number i) = n i
foldExpr n v c j p f o cp d s (Variable i) = v i
foldExpr n v c j p f o cp d s (Closure i args e) = c i args (foldExpr n v c j p f o cp d s e)
foldExpr n v c j p f o cp d s (e1 :> e2) = j (foldExpr n v c j p f o cp d s e1) (foldExpr n v c j p f o cp d s e2)
foldExpr n v c j p f o cp d s (Prim i)   = p i
foldExpr n v c j p f o cp d s (If e1 e2) = f (foldExpr n v c j p f o cp d s e1) (foldExpr n v c j p f o cp d s e2)
foldExpr n v c j p f o cp d s (Nop) = o
foldExpr n v c j p f o cp d s (CodePop i) = cp i
foldExpr n v c j p f o cp d s (Deref i) = d i
foldExpr n v c j p f o cp d s (Set i) = s i


}
