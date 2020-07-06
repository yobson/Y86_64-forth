{
module Compiler.Parse
( buildExpr
, Expr(..)
, Value(..)
, Statement(..)
, CodeGen(..)
, foldExpr
, foldExprVal
, foldValue
, foldStmt
, foldCodeGen
, flattern
) where

import qualified Compiler.Lex as L
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

Exp  : SExp Exp                     { $1 :> $2 }
     | SExp                         { $1 }

SExp : Value                        { Val $1 }
     | ':' id Exp ';'               { Closure $2 [] $3 }
     | ':'  id '{' List '}' Exp ';' { Closure $2 $4 $6 }
     | Stmt                         { Stmt $1 }

Value : num                         { Number $1 }
      | id                          { Variable $1 }
      | prim                        { Prim $1 }
      | id '@'                      { Deref $1 }

Stmt : if '{' Exp '}' '{' Exp '}'   { If $3 $6 }
     | nop                          { Nop }
     | id '!'                       { Set $1 }

List : id                           { [$1] }
     | id List                      { $1:$2 }

{
type Tokens = L.Token
parseError :: [Tokens] -> a
parseError xs = error ("Parse error" ++ show xs)

type Ident = String
type Stack = [Int]
type Env   = [(Ident, Expr)]

data Expr = Val Value
          | Closure Ident [Ident] Expr
          | CG CodeGen
          | Stmt Statement
          | Expr :> Expr

infixr 6 :>

data Statement = If Expr Expr | Set Ident | Nop deriving Show

data Value = Variable Ident 
           | Number Int 
           | Prim Ident 
           | Deref Ident
           deriving Show

-- fake instructions for codeGen only
data CodeGen = CodePop Int deriving Show

instance Show Expr where
  show (Closure name pat e) = concat ["Def ", name, "", show pat, "\n", show e, "\nEND ", name]
  show (Val v) = show v
  show (CG c)  = show c
  show (Stmt s) = show s
  show (e1 :> e2) = concat [show e1, "\n", show e2]


instance Semigroup (Expr) where
  (<>) (x :> xs) e = x :> (xs <> e)
  (<>)  x        e = x :> e

foldExpr :: (Value -> a) 
         -> (Ident -> [Ident] -> a -> a) 
         -> (CodeGen -> a) 
         -> (Statement -> a) 
         -> (a -> a -> a) 
         -> Expr -> a
foldExpr val closure cg stmt next (Val v)            = val v
foldExpr val closure cg stmt next (Closure i args e) = closure i args $ foldExpr val closure cg stmt next e
foldExpr val closure cg stmt next (CG c)             = cg c
foldExpr val closure cg stmt next (Stmt s)           = stmt s
foldExpr val closure cg stmt next (e1 :> e2)         = fld e1 `next` fld e2
  where fld = foldExpr val closure cg stmt next

foldStmt :: (Expr -> Expr -> a) -> (Ident -> a) -> a -> Statement -> a
foldStmt if' set nop (If e1 e2) = if' e1 e2
foldStmt if' set nop (Set i)    = set i
foldStmt if' set nop (Nop)      = nop

foldValue :: (Ident -> a) -> (Int -> a) -> (Ident -> a) -> (Ident -> a) -> Value -> a
foldValue variable number prim deref (Variable i) = variable i
foldValue variable number prim deref (Number i)   = number i
foldValue variable number prim deref (Prim i)     = prim i
foldValue variable number prim deref (Deref i)    = deref i

foldCodeGen :: (Int -> a) -> CodeGen -> a
foldCodeGen codePop (CodePop i) = codePop i

foldExprVal f = foldExpr f Closure CG stmt (:>)
  where stmt = Stmt . foldStmt (\x y -> If (foldExprVal f x) (foldExprVal f y)) Set Nop

flattern :: Expr -> Expr
flattern = foldr1 (\x xs -> x :> xs) . flattern'

flattern' :: Expr -> [Expr]
flattern' (x :> y) = concat [flattern' x, flattern' y]
flattern' x        = [x]

}
