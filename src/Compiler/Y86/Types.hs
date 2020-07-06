{-# LANGUAGE CPP, StandaloneDeriving #-}

module Compiler.Y86.Types 
( Reg
, Mem(..)
, St

, AsmExpr(..)
, codeMap

, Asm(..)
, unAsm

, M
, getVar
, setVar
, newVar
, mkLabel
, runM
) where

import Control.Monad.State.Lazy
import Data.Bifunctor

type Reg = Int
data Mem = RPtr Reg | Absolute Int deriving (Show, Eq)

type St = (Int, [(String, Int)])
type M a = State St a

runM = runState

findM :: St -> String -> Maybe Int
findM (_, [])    s = Nothing
findM (_,((x,i):xs)) s | x == s    = Just i
                       | otherwise = findM (undefined, xs) s

find :: St -> String -> Int
find xs s = case findM xs s of
              Nothing  -> error $ "Undefined variable " ++ s
              (Just x) -> x

eqTup b (a,_) = a == b

getVar :: String -> State St Asm
getVar s = get >>= \xs -> return $ Code $ Irmovq (find xs s) 0 :> Mrmovq (RPtr 0) 0 :> Pushq 0

setVar :: String -> State St Asm
setVar s = get >>= \xs -> case (findM xs s) of
                                (Nothing) -> newVar s >>= (\x -> return $ Code $ Popq 0 :> Irmovq x 2 :> Rmmovq 0 (RPtr 2))
                                (Just x)  -> return $ Code $ Popq 0 :> Irmovq x 2 :> Rmmovq 0 (RPtr 2)

newVar :: String -> State St Int
newVar s = nextVar >>= (\x -> modify (bimap id ((s,x):)) >> return x)

nextVar :: State St Int
nextVar = get >>= return . f
  where f (_,[])       = 0x200
        f (_,(x,v):xs) = v + 8

mkLabel :: State St String
mkLabel = modify (bimap (+1) id) >> get >>= (\x -> return $ "label" ++ (show $ fst x))

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

codeMap :: (a -> AsmExpr) -> [a] -> AsmExpr
codeMap f [] = Nop
codeMap f xs = foldr1 (:>) $ map f xs
