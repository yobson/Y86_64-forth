{-# LANGUAGE PatternSynonyms, StandaloneDeriving, CPP #-}

module Y86 (genCode) where

import qualified Parse as P
import Control.Monad.State.Lazy

type Reg = Int
data Mem = RPtr Reg | Absolute Int deriving (Show, Eq)

data Asm = Func AsmExpr | Code AsmExpr
unAsm (Func x) = x
unAsm (Code x) = x

instance Show Asm where
  show (Func i) = show i
  show (Code i) = show i

data AsmExpr = Halt
          | Nop
          | Rrmovq Reg Reg
          | Irmovq Int Reg
          | Rmmovq Reg Mem
          | Mrmovq Mem Reg
          | Addq Reg Reg
          | Subq Reg Reg
          | Pushq Reg
          | Popq Reg
          | Label String
          | Retq
          | Je String
          | Jmp String
          | Callq String
          | HardCode String
          | AsmExpr :> AsmExpr
          deriving Eq

infixr 6 :>

reg2String :: Int -> String
reg2String 0 = "%rax"
reg2String 1 = "%rbx"
reg2String 2 = "%rcx"
reg2String 3 = "%rdx"
reg2String 4 = "%rsi"
reg2String 5 = "%rdi"
reg2String 42 = "%rsp"
reg2String 69 = "%rbp"
reg2String 100 = "%r8"
reg2String i = "%r" ++ show (i+3)

mem2String :: Mem -> String
mem2String (RPtr i)     = concat ["(", reg2String i, ")"]
mem2String (Absolute i) = '$': (show i)

pattern BinOP1 i j op = (P.Number i P.:> (P.Number j P.:> P.Prim op))
pattern BinOP2 j op = (P.Number j P.:> P.Prim op)
pattern BinOP1' i j op xs = (P.Number i P.:> (P.Number j P.:> (P.Prim op P.:> xs)))
pattern BinOP2' j op xs = (P.Number j P.:> (P.Prim op P.:> xs))

postCall = Subq 8 69 :> Popq 100 :> Rmmovq 100 (RPtr 69)
preReturn = Mrmovq (RPtr 69) 100 :> Pushq 100 :> Addq 8 69

#ifndef DEBUG
instance Show AsmExpr where
  show Nop            = []
  show (Halt)         = "\tHalt"
  show (Rrmovq r1 r2) = "\trrmovq " ++ reg2String r1 ++ (',' : ' ' : reg2String r2)
  show (Irmovq v r)   = "\tirmovq " ++ show v ++ (',' : ' ' : reg2String r)
  show (Mrmovq d r)   = "\tmrmovq " ++ mem2String d ++ ( ',' : ' ' : reg2String r)
  show (Rmmovq r m)   = "\trmmovq " ++ reg2String r ++ (',' : ' ' : mem2String m)
  show (Addq r1 r2)   = "\taddq " ++ reg2String r1 ++ (',': ' ' : reg2String r2)
  show (Subq r1 r2)   = "\tsubq " ++ reg2String r1 ++ (',' : ' ' : reg2String r2)
  show (Pushq r)      = "\tpushq " ++ reg2String r
  show (Popq r)       = "\tpopq " ++ reg2String r
  show (Label l)      = l ++ ":"
  show (Retq)         = "\tret"
  show (Je s)         = "\tje " ++ s
  show (Jmp s)        = "\tjmp " ++ s
  show (Callq s)      = "\tcall " ++ s
  show (e1 :> e2)     = show e1 ++ ('\n':show e2)
  show (HardCode s)   = s
#else
deriving instance Show AsmExpr
#endif

genCode :: P.Expr -> String
genCode e = show $ addHardCode $ fullOpt $ unAsm $ fst $ runState (codeGen e) 0
  
codeMap :: (a -> AsmExpr) -> [a] -> AsmExpr
codeMap f [] = Nop
codeMap f xs = foldr1 (:>) $ map f xs

codeGen :: P.Expr -> State Int Asm
codeGen (P.Nop) = return $ Code Nop

codeGen (P.Closure i args e P.:> xs) | length' e < 5 = do 
  let c = renameClosure 2 args e
  let c' = foldr (\x xs -> P.CodePop x P.:> xs) P.Nop [2..(1 + length args)] P.:> c
  codeGen (inline i c' xs)

codeGen (P.Closure i args e) = do 
    c <- codeGen' (renameClosure 2 args e)
    return $ Func $ Label i :> postCall :> (codeMap (\(i,_) -> Popq i) $ zip [2..] args) :> c :> preReturn :> Retq

codeGen (BinOP1' i j op xs) = codeGen (BinOP1 i j op P.:> xs)

codeGen (BinOP2' j op xs) = codeGen (BinOP2 j op P.:> xs)

codeGen (BinOP1 i j op) = return $ Code $ Irmovq i 0 :> Irmovq j 1 :> primCodeGen' op :> Pushq 0

codeGen (BinOP2 j op) = return $ Code $ Popq 0 :> Irmovq j 1 :> primCodeGen' op :> Pushq 0

codeGen (P.Number i) = return $ Code $ Irmovq i 0 :> Pushq 0

codeGen (P.Prim i) = return $ Code $ primCodeGen i

codeGen (P.CodePop i) = return $ Code $ Popq i

codeGen (P.If e1 e2) = do 
    exp1 <- codeGen' e1
    exp2 <- codeGen' e2
    lab1 <- mkLabel
    lab2 <- mkLabel
    end  <- mkLabel
    return $ Code $ Popq 0 :> Irmovq 0 1 :> Subq 0 1 :> Je lab1 :> Jmp lab2 :> Label lab1 :> exp1 :> Jmp end :> Label lab2 :> exp2 :> Label end

codeGen (e1 P.:> e2) = do
    p1 <- codeGen e1
    p2 <- codeGen e2
    if (ifFunc p2) then return $ Code (ret p2 :> ret p1) else return $ Code (ret p1 :> ret p2)
  where ret (Func i) = i
        ret (Code i) = i

codeGen (P.Variable i) | (head i) `elem` ['0'..'9'] = return $ Code $ Pushq $ read i
                       | otherwise                  = return $ Code $ Callq i

codeGen' e = codeGen e >>= return . unAsm

primCodeGen i = Popq 0 :> Popq 1 :> primCodeGen' i :> Pushq 0

primCodeGen' "+"  = Addq 1 0
primCodeGen' "-"  = Subq 1 0
primCodeGen' "==" = Subq 1 0

renameClosure :: Int -> [String] -> P.Expr -> P.Expr
renameClosure i [] e = e
renameClosure i (x:xs) e = renameClosure (i+1) xs $ P.foldExpr P.Number (varReplace i x) P.Closure (P.:>) P.Prim P.If P.Nop P.CodePop e

varReplace :: Int -> String -> String -> P.Expr
varReplace i x y | x == y    = P.Variable (show i)
                 | otherwise = P.Variable y

inline :: String -> P.Expr -> P.Expr -> P.Expr
inline i e = P.foldExpr P.Number sub P.Closure (P.:>) P.Prim P.If P.Nop P.CodePop
  where sub s | s == i    = e
              | otherwise = P.Variable s

ifFunc (Func i) = True
ifFunc _        = False

mkLabel :: State Int String
mkLabel = modify (+1) >> get >>= (\x -> return $ "label" ++ show x)

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
optimise (Irmovq i r :> Pushq d1 :> Popq d2 :> xs)       = Irmovq i d2 :> optimise xs
optimise (Irmovq i1 r1 :> Irmovq i2 r2 :> xs) | r1 == r2 = Irmovq i2 r2 :> optimise xs
optimise (Irmovq i1 r1 :> Rrmovq r2 r3 :> xs) | r1 == r3 = Irmovq i1 r3 :> optimise xs
optimise (Popq r1 :> Rrmovq r2 r3 :> xs)                 = Popq r3 :> optimise xs
optimise (Pushq i :> Popq j :> xs)            | i == j   = optimise xs
optimise (Popq i :> Pushq j :> xs)            | i == j   = Mrmovq (RPtr 42) j :> optimise xs 
optimise (Pushq i :> Popq j :> xs)                       = Rrmovq i j :> optimise xs
optimise (Jmp i :> Jmp j :> xs)                          = Jmp i :> optimise xs
optimise (Jmp i :> Label s :> xs)             | i == s   = Label s :> optimise xs
optimise (Nop :> y)                                      = optimise y
optimise (x :> Nop)                                      = x
optimise (x :> xs)                                       = x :> optimise xs 
optimise  x                                              = x

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
unUsedLabel s (Callq i) = s /= i
unUsedLabel s (x :> xs) = unUsedLabel s x && unUsedLabel s xs
unUsedLabel _ x         = True

length' (x P.:> y) = 1 + length' y
length' (P.If x y) = 1 + length' x + length' y
length' x = 1

topCode :: AsmExpr
topCode = HardCode ".pos 0\n\tirmovq stack, %rsp\n\tirmovq base, %rbp" :> Irmovq 8 8 :> Jmp "main" :> Nop

mainDef :: AsmExpr
mainDef = Label "main"

bottomCode :: AsmExpr
bottomCode = HardCode "halt\n\n.pos 0x200\nstack:\n\n.pos 0x1b0\nbase:\n"

addHardCode :: AsmExpr -> AsmExpr
addHardCode asm = topCode :> addMain asm :> bottomCode

addMain :: AsmExpr -> AsmExpr
addMain (Retq :> Label r :> xs) = Retq :> Label r :> addMain xs
addMain (Retq :> xs)            = Retq :> mainDef :> xs
addMain (x :> xs)               = x :> addMain xs
addMain x                       = x
