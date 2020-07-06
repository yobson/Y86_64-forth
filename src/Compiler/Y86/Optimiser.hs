module Compiler.Y86.Optimiser
( fullOpt
, flattern
) where

import Compiler.Y86.Types

fullOpt = dropUntilSame . iterate (optimise . flattern . labOpt Nop)

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

optimise :: AsmExpr -> AsmExpr
optimise (Irmovq i r :> Pushq d1 :> Popq d2 :> xs) | r == d1  = Irmovq i d2 :> optimise xs
optimise (Irmovq i1 r1 :> Irmovq i2 r2 :> xs)      | r1 == r2 = Irmovq i2 r2 :> optimise xs
optimise (Irmovq i1 r1 :> Rrmovq r2 r3 :> xs)      | r1 == r3 = Irmovq i1 r3 :> optimise xs
optimise (Mrmovq m r1 :> Rrmovq r2 r3 :> xs)       | r1 == r2 = Mrmovq m r3  :> optimise xs
optimise (Popq r1 :> Rrmovq r2 r3 :> xs)                      = Popq r3 :> optimise xs
optimise (Pushq i :> Popq j :> xs)                 | i == j   = optimise xs
optimise (Popq i :> Pushq j :> xs)                 | i == j   = Mrmovq (RPtr 42) j :> optimise xs 
optimise (Pushq i :> Popq j :> xs)                            = Rrmovq i j :> optimise xs
optimise (Jmp i :> Jmp j :> xs)                               = Jmp i :> optimise xs
optimise (Jmp i :> Label s :> xs)                  | i == s   = Label s :> optimise xs
optimise (Nop :> y)                                           = optimise y
optimise (x :> Nop)                                           = x
optimise (x :> xs)                                            = x :> optimise xs 
optimise  x                                                   = x

labOpt :: AsmExpr -> AsmExpr -> AsmExpr
labOpt old (Label i :> Jmp j :> xs)   = labOpt ((chJmp i j old) :> Jmp j) (chJmp i j xs)
labOpt old (Label i :> xs)            | unUsedLabel i (old :> xs) = labOpt old xs
labOpt old (Label i :> Label j :> xs) = labOpt ((chJmp i j old) :> Label j) (chJmp i j xs)
labOpt old (x :> xs)                  = labOpt (old :> x) xs
labOpt old x                          = old :> x

chJmp :: String -> String -> AsmExpr -> AsmExpr
chJmp i j (Jmp k) | i == k = Jmp j
chJmp i j (Je k)  | i == k = Je j
chJmp i j (x :> xs)        = chJmp i j x :> chJmp i j xs
chJmp _ _  x               = x

unUsedLabel :: String -> AsmExpr -> Bool
unUsedLabel s (Jmp i)   = s /= i
unUsedLabel s (Je  i)   = s /= i
unUsedLabel s (Callq i) = False
unUsedLabel s (x :> xs) = unUsedLabel s x && unUsedLabel s xs
unUsedLabel _ x         = True
