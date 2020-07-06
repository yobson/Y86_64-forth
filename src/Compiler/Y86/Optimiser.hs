{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

module Compiler.Y86.Optimiser
( fullOpt
, flattern
, transformForth
) where

import qualified Compiler.Parse as P
import           Compiler.Y86.Types

functionCode i xs = Label i :>  Subq 8 69 :> Popq 100 :> Rmmovq 100 (RPtr 69) :> xs
pattern Function i xs = Label i :>  Subq 8 69 :> Popq 100 :> Rmmovq 100 (RPtr 69) :> xs

fullOpt = dropUntilSame . iterate (optimise . removeNops . flattern . labOpt Nop) . flattern

dropUntilSame :: (Eq a) => [a] -> a
dropUntilSame []       = error "!!!"
dropUntilSame [x]      = x
dropUntilSame (x:y:xs) | x == y    = x
                       | otherwise = dropUntilSame (y:xs)

flattern :: AsmExpr -> AsmExpr
flattern = foldr1 (\x xs -> x :> xs) . flattern'

flattern' :: AsmExpr -> [AsmExpr]
flattern' (x :> y) = concat [flattern' x, flattern' y]
flattern' x        = [x]

removeNops (Nop :> y) = removeNops y
removeNops (x :> Nop) = x
removeNops (x :> xs)  = x :> removeNops xs 
removeNops  x         = x

optimise :: AsmExpr -> AsmExpr
optimise (Irmovq i r :> Pushq d1 :> Popq d2 :> xs) | r == d1  = Irmovq i d2 :> optimise xs
optimise (Irmovq i1 r1 :> Irmovq i2 r2 :> xs)      | r1 == r2 = Irmovq i2 r2 :> optimise xs
optimise (Irmovq i1 r1 :> Rrmovq r2 r3 :> xs)                 = Irmovq i1 r3 :> optimise xs
optimise (Mrmovq m r1 :> Rrmovq r2 r3 :> xs)       | r1 == r2 = Mrmovq m r3  :> optimise xs
optimise (Popq r1 :> Rrmovq r2 r3 :> xs)                      = Popq r3 :> optimise xs
optimise (Pushq i :> Popq j :> xs)                 | i == j   = optimise xs
optimise (Popq i :> Pushq j :> xs)                 | i == j   = Mrmovq (RPtr 42) j :> optimise xs 
optimise (Popq i :> Popq j :> Pushq k :> Rrmovq a b :> xs)     | j == a   = Popq i :> Popq b :> Pushq k :> optimise xs 
optimise (Pushq i :> Popq j :> xs)                            = Rrmovq i j :> optimise xs
optimise (Jmp i :> Jmp j :> xs)                               = Jmp i :> optimise xs
optimise (Jmp i :> Label s :> xs)                  | i == s   = Label s :> optimise xs
optimise (x :> xs)                                            = x :> optimise xs 
optimise  x                                                   = x
 
labOpt :: AsmExpr -> AsmExpr -> AsmExpr
labOpt old (Label i :> Jmp j :> xs)     = labOpt ((chJmp i j old) :> Jmp j) (chJmp i j xs)
labOpt old (Label i :> xs)              | unUsedLabel i (old :> xs) = labOpt old xs
labOpt old (Label i :> Label j :> xs)   = labOpt ((chJmp i j old) :> Label j) (chJmp i j xs)
labOpt old (Function i (Popq r :> xs))  | checkForPush i xs =  labOpt (old :> functionCode i Nop) (chCall i r xs)
labOpt old (Function i (Mrmovq (RPtr 42) r :> xs)) | checkForPush i xs = labOpt (old :> functionCode i (Pushq r)) (chCall i r xs)
labOpt old (x :> xs)                    = labOpt (old :> x) xs
labOpt old x                            = old :> x

chJmp :: String -> String -> AsmExpr -> AsmExpr
chJmp i j (Jmp k) | i == k = Jmp j
chJmp i j (Je k)  | i == k = Je j
chJmp i j (x :> xs)        = chJmp i j x :> chJmp i j xs
chJmp _ _  x               = x

chCall i r (Pushq r' :> Callq i' :> xs) | i' == i = Rrmovq r' r :> Callq i :> chCall i r xs
chCall i r (Pushq r' :> Callq i')       | i' == i = Rrmovq r' r :> Callq i
chCall i r (x :> xs)                             = x :> chCall i r xs
chCall i r x                                     = x

checkForPush i (Pushq r' :> Callq i' :> xs) = i' == i  && checkForPush i xs
checkForPush i (Pushq r' :> Callq i')       = i' == i
checkForPush i (Callq i' :> xs)             = i' /= i  && checkForPush i xs
checkForPush i (x :> xs)                    = True     && checkForPush i xs
checkForPush i x                            = True

unUsedLabel :: String -> AsmExpr -> Bool
unUsedLabel s (Jmp i)   = s /= i
unUsedLabel s (Je  i)   = s /= i
unUsedLabel s (Callq i) = False
unUsedLabel s (x :> xs) = unUsedLabel s x && unUsedLabel s xs
unUsedLabel _ x         = True

pattern PNop               =  P.Stmt (P.Nop)
pattern Prim i             =  P.Val (P.Prim i)
pattern ArgsVV  i v1 v2 xs <- (v1@(P.Val v1') P.:> v2@(P.Val v2') P.:> Prim i P.:> xs)
pattern ArgsCV  i v1 v2 xs <- (v1@(P.CG v1')  P.:> v2@(P.Val v2') P.:> Prim i P.:> xs)
pattern ArgsVC  i v1 v2 xs <- (v1@(P.Val v1') P.:> v2@(P.CG v2')  P.:> Prim i P.:> xs)
pattern ArgsCC  i v1 v2 xs <- (v1@(P.CG v1')  P.:> v2@(P.CG v2')  P.:> Prim i P.:> xs)
pattern ArgsVV' i v1 v2    <- (v1@(P.Val v1') P.:> v2@(P.Val v2') P.:> Prim i)
pattern ArgsCV' i v1 v2    <- (v1@(P.CG v1')  P.:> v2@(P.Val v2') P.:> Prim i)
pattern ArgsVC' i v1 v2    <- (v1@(P.Val v1') P.:> v2@(P.CG v2')  P.:> Prim i)
pattern ArgsCC' i v1 v2    <- (v1@(P.CG v1')  P.:> v2@(P.CG v2')  P.:> Prim i)

transformForth :: P.Expr -> P.Expr
transformForth (ArgsVV  i v1 v2 xs) = (P.CG $ P.BinOp i v1 v2) P.:> transformForth xs
transformForth (ArgsCV  i v1 v2 xs) = (P.CG $ P.BinOp i v1 v2) P.:> transformForth xs
transformForth (ArgsVC  i v1 v2 xs) = (P.CG $ P.BinOp i v1 v2) P.:> transformForth xs
transformForth (ArgsCC  i v1 v2 xs) = (P.CG $ P.BinOp i v1 v2) P.:> transformForth xs
transformForth (ArgsVV' i v1 v2   ) = (P.CG $ P.BinOp i v1 v2)
transformForth (ArgsCV' i v1 v2   ) = (P.CG $ P.BinOp i v1 v2)
transformForth (ArgsVC' i v1 v2   ) = (P.CG $ P.BinOp i v1 v2)
transformForth (ArgsCC' i v1 v2   ) = (P.CG $ P.BinOp i v1 v2)
transformForth (x P.:> xs)         = x P.:> transformForth xs
transformForth x                   = x


