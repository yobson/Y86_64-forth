module Interpreter
( eval
, Env
, Stack
, Mem
, M
) where

import Compiler.Parse
import Control.Monad.State.Lazy

type Ident = String

type Stack = [Int]
type Env   = [(Ident, Expr)]
type Mem   = [(Ident, Int)]

type M a = State Mem a

(//) :: [a] -> ((a -> Bool), a) -> [a]
[]     // (f, a) = [a]
(x:xs) // (f, a) | f x       = a : xs
                | otherwise = x : (xs // (f, a))

infixl 5 //

eqTup b (a,_) = a == b

findM :: Ident -> M Int
findM i = get >>= return . snd . head . filter (eqTup i)

set :: Ident -> Int -> M ()
set ide i = modify (// (eqTup ide, (ide, i)))

eval :: Expr -> Stack -> Env -> M (Stack,Env)
eval (Val v)               xs env = evalValue v xs env
eval (Stmt s)              xs env = evalStmt  s xs env
eval cl@(Closure i args e) xs env = return $ (xs, elab env i cl)
eval (e1 :> e2)            xs env = eval e1 xs env >>= uncurry (eval e2)

evalStmt :: Statement -> Stack -> Env -> M (Stack,Env)
evalStmt (Nop)         xs  env = return $ (xs,env)
evalStmt (If e1 e2) (0:xs) env = eval e1 xs env
evalStmt (If e1 e2) (_:xs) env = eval e2 xs env
evalStmt (Set i)    (x:xs) env = set i x >> return (xs,env)

evalValue :: Value -> Stack -> Env -> M (Stack,Env)
evalValue (Number i)   xs  env = return $ (i:xs,env)
evalValue (Prim i)     xs  env = return $ (primEval i xs, env)
evalValue (Deref i)    xs  env = findM i >>= \x -> return (x:xs, env)
evalValue (Variable i) xs  env = case (find i env) of
                              (Closure i args expr) -> do 
                                     let (env',s) = rename args xs env
                                     (s',_)       <- eval expr s env' 
                                     return (s', env)
                              (e)                   -> eval e xs env



primEval :: Ident -> Stack -> Stack
primEval "+" (x:y:xs)  = (x+y):xs
primEval "-" (x:y:xs)  = (y-x):xs
primEval "*" (x:y:xs)  = (x*y):xs
primEval "==" (x:y:xs) | (x == y)  = 0:xs
                       | otherwise = -1:xs
primEval "<=" (x:y:xs) | (x <= y)  = 0:xs
                       | otherwise = -1:xs
primEval ">=" (x:y:xs) | (x >= y)  = 0:xs
                       | otherwise = -1:xs
primEval "<"  (x:y:xs) | (x <  y)  = 0:xs
                       | otherwise = -1:xs
primEval ">"  (x:y:xs) | (x >  y)  = 0:xs
                       | otherwise = -1:xs
primEval "/" (x:y:xs)  = (y `div` x):xs
primEval "%" (x:y:xs)  = (y `mod` x):xs

rename :: [Ident] -> Stack -> Env -> (Env, Stack)
rename [] ss env = (env, ss)
rename (i:ids) (s:ss) env = (elab l i (Val $ Number s), r)
  where (l,r) = rename ids ss env

find :: Ident -> Env -> Expr
find i e = snd $ head $ filter (\(n,_) -> i == n) e

elab :: Env -> Ident -> Expr -> Env
elab env i e = (i,e):env
