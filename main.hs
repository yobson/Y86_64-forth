module Main (Expr, main) where

import qualified Lex as L
import Parse
import qualified Y86 as Y
import System.Environment

type Ident = String


type Stack = [Int]
type Env   = [(Ident, Expr)]

eval :: Expr -> Stack -> Env -> (Stack,Env)
eval (Nop)                 xs  env = (xs,env)
eval (Number i)            xs  env = (i:xs,env)
eval cl@(Closure i args e) xs  env = (xs, elab env i cl)
eval (Prim i)              xs  env = (primEval i xs, env)
eval (If e1 e2)         (0:xs) env = eval e1 xs env
eval (If e1 e2)         (_:xs) env = eval e2 xs env
eval (e1 :> e2)            xs  env = let (s1,env1) = eval e1 xs env in eval e2 s1 env1
eval (Variable i)          xs  env = case (find i env) of
                                       (Closure i args expr) -> let (env',s) = rename args xs env in 
                                                                let (s',_)    = eval expr s env'   in (s', env)
                                       (e)                   -> eval e xs env

data Arch = Y86



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
rename (i:ids) (s:ss) env = (elab l i (Number s), r)
  where (l,r) = rename ids ss env

find :: Ident -> Env -> Expr
find i e = snd $ head $ filter (\(n,_) -> i == n) e

elab :: Env -> Ident -> Expr -> Env
elab env i e = (i,e):env

runLoop :: Stack -> Env -> IO ()
runLoop s e = do
      expr <- getLine >>= return . buildExpr . L.tokenise
      let (s',e') = eval expr s e
      print s'
      runLoop s' e'

main = do
  args <- getArgs 
  if (args == []) then runLoop [] [] else getContents >>= return . buildExpr . L.tokenise >>= return . Y.genCode >>= putStrLn
