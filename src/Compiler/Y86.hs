{-# LANGUAGE PatternSynonyms #-}

module Compiler.Y86 (genCode) where

import qualified Compiler.Parse            as P
import           Compiler.Y86.Types
import           Compiler.Y86.Optimiser


genCode :: Bool -> Int -> P.Expr -> String
genCode opt bp e = show $ addHardCode bp $ (if opt then fullOpt else flattern) $ unAsm $ fst $ runM (codeGen e) (0,[])
  
pattern BinOP1 i j op = (P.Val (P.Number i) P.:> P.Val (P.Number j) P.:> P.Val (P.Prim op))
pattern BinOP2 j op = (P.Val (P.Number j) P.:> P.Val (P.Prim op))
pattern BinOP1' i j op xs = (P.Val (P.Number i) P.:> P.Val (P.Number j) P.:> P.Val (P.Prim op) P.:> xs)
pattern BinOP2' j op xs = (P.Val (P.Number j) P.:> P.Val (P.Prim op) P.:> xs)

postCall = Subq 8 69 :> Popq 100 :> Rmmovq 100 (RPtr 69)
preReturn = Mrmovq (RPtr 69) 100 :> Pushq 100 :> Addq 8 69

codeGen :: P.Expr -> M Asm
codeGen (BinOP1' i j op xs) = codeGen (BinOP1 i j op P.:> xs)
codeGen (BinOP2' j op xs) = codeGen (BinOP2 j op P.:> xs)
codeGen (BinOP1 i j op) = return $ Code $ Irmovq i 0 :> Irmovq j 1 :> primCodeGen' op :> Pushq 0
codeGen (BinOP2 j op) = return $ Code $ Popq 0 :> Irmovq j 1 :> primCodeGen' op :> Pushq 0
codeGen (P.Closure i args e P.:> xs) | length' e < 8 && notRec e i = do 
  let c = renameClosure 2 args e
  let c' = foldr (\x xs -> (P.CG $ P.CodePop x) P.:> xs) (P.Stmt P.Nop) [2..(1 + length args)] P.:> c
  codeGen (inline i c' xs)
codeGen x = codeGenNoOpt x

codeGen' e = codeGen e >>= return . unAsm

codeGenNoOpt (P.Val v)  = codeGenValue v
codeGenNoOpt (P.Stmt s) = codeGenStmt s
codeGenNoOpt (P.CG c)   = codeGenCG c

codeGenNoOpt (P.Closure i args e) = do 
    c <- codeGen' (renameClosure 2 args e)
    return $ Func $ Label i :> postCall :> (codeMap (\(i,_) -> Popq i) $ zip [2..] args) :> c :> preReturn :> Retq

codeGenNoOpt (e1 P.:> e2) = do
    p1 <- codeGen e1
    p2 <- codeGen e2
    if (ifFunc p2) then return $ Code (p2 |> p1) else return $ Code (p1 |> p2)
  where x |> y = unAsm x :> unAsm y


codeGenStmt (P.Nop)   = return $ Code Nop
codeGenStmt (P.Set i) = setVar i

codeGenStmt (P.If e1 e2) = do 
    exp1 <- codeGen' e1
    exp2 <- codeGen' e2
    lab1 <- mkLabel
    lab2 <- mkLabel
    end  <- mkLabel
    return $ Code $ Popq 0 :> Irmovq 0 1 :> Subq 0 1 :> Je lab1 :> Jmp lab2 :> Label lab1 :> exp1 :> Jmp end :> Label lab2 :> exp2 :> Label end

codeGenValue (P.Number i) = return $ Code $ Irmovq i 0 :> Pushq 0
codeGenValue (P.Prim i)   = return $ Code $ primCodeGen i
codeGenValue (P.Deref i)  = getVar i

codeGenValue (P.Variable i) | (head i) `elem` ['0'..'9'] = return $ Code $ Pushq $ read i
                            | otherwise                  = return $ Code $ Callq i

codeGenCG (P.CodePop i) = return $ Code $ Popq i


primCodeGen i = Popq 0 :> Popq 1 :> primCodeGen' i :> Pushq 0

primCodeGen' "+"  = Addq 1 0
primCodeGen' "-"  = Subq 1 0
primCodeGen' "==" = Subq 1 0

renameClosure :: Int -> [String] -> P.Expr -> P.Expr
renameClosure i [] e = e
renameClosure i (x:xs) e = renameClosure (i+1) xs $ P.foldExprVal (varReplace i x) e

varReplace :: Int -> String -> P.Value -> P.Expr
varReplace i x (P.Variable y) | x == y    = P.Val $ P.Variable (show i)
                              | otherwise = P.Val $ P.Variable y
varReplace _ x f                          = P.Val $ f

inline :: String -> P.Expr -> P.Expr -> P.Expr
inline i e = P.foldExprVal sub
  where sub (P.Variable s) | s == i = e
        sub v                       = P.Val v

ifFunc (Func i) = True
ifFunc _        = False

length' (x P.:> y)          = 1 + length' y
length' (P.Stmt (P.If x y)) = 1 + length' x + length' y
length' x                   = 1

false = const False

notRec :: P.Expr -> String -> Bool
notRec e i = not $ P.foldExpr (P.foldValue (== i) false false false) (const $ const false) false false (||) e

topCode :: AsmExpr
topCode = HardCode ".pos 0\n\tirmovq stack, %rsp\n\tirmovq base, %rbp" :> Irmovq 8 8 :> Jmp "main" :> Nop

mainDef :: AsmExpr
mainDef = Label "main"

bottomCode :: Int -> AsmExpr
bottomCode basePtr = HardCode $"halt\n\n.pos 0x200\nstack:\n\n.pos " ++ show basePtr ++ "\nbase:\n"

addHardCode :: Int -> AsmExpr -> AsmExpr
addHardCode bp asm = topCode :> addMain asm :> bottomCode bp

addMain :: AsmExpr -> AsmExpr
addMain (Retq :> Label r :> xs) = Retq :> Label r :> addMain xs
addMain (Retq :> xs)            = Retq :> mainDef :> xs
addMain (x :> xs)               = x :> addMain xs
addMain x                       = x
