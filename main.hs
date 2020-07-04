module Main (Expr, main) where

import System.IO
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


type HelpText = String

data Argument = Flag String | Input String | Switch Char

data Plan = Interactive | Compile deriving (Show, Eq)

data CompileSettings = CompSet { mode       :: Plan
                               , outputFile :: Maybe FilePath
                               , inFile     :: Maybe FilePath
                               , opt        :: Bool
                               }
                            deriving (Show)

type Arg = (Argument, HelpText, CompileSettings -> String -> CompileSettings)

initialSettings = CompSet Compile Nothing Nothing True

turnOffOpt, setOutFile, setInFile, setRepl :: CompileSettings -> String -> CompileSettings
turnOffOpt s _ = s {opt        = False}
setOutFile s f = s {outputFile = Just f}
setInFile  s f = s {inFile     = Just f}
setRepl    s _ = s {mode       = Interactive}

extractHelp :: Arg -> String
extractHelp f@(_, h, _) = concat ["--", long f, "\t-", short f, "\t", h]

printHelp :: [Arg] -> String
printHelp = foldr (\x xs -> extractHelp x ++ '\n':xs) []

short :: Arg -> String
short ((Flag  (s:ss)),_,_) = [s]
short ((Input (s:ss)),_,_) = [s]
short ((Switch c),_,_)     = [c]

long :: Arg -> String
long ((Flag   s),_,_)  = s
long ((Input  s),_,_)  = s
long ((Switch _),_,_)  = ""

globalArgs = [ (Flag "interactive", "Run Repl", setRepl)
             , (Input "output", "Set output file name", setOutFile)
             , (Flag "no-opt", "Turn off optimisation", turnOffOpt)
             ]

match :: Arg -> [String] -> CompileSettings -> CompileSettings
match (Flag  i, _, f) ys     c = parseArgs ys $ f c ""
match (Input s, _, f) (y:ys) c = parseArgs ys $ f c y
match (Switch s,_, f) ys     c = parseArgs ys $ f c ""

findMatch :: (Arg -> String) -> [Arg] -> String -> [String] -> CompileSettings -> CompileSettings
findMatch m []               s ys c            = error $ "Unknown argument " ++ s
findMatch m (arg:xs) s ys c | m arg == s  = match arg ys c
                            | otherwise = findMatch m xs s ys c

parseArgs :: [String] -> CompileSettings -> CompileSettings
parseArgs []                c = c
parseArgs (['-']:xs)        c = parseArgs xs c
parseArgs (('-':'-':xs):ys) c = findMatch long globalArgs xs ys c
parseArgs (('-':x:xs):ys)   c = parseArgs [('-':xs)] $ findMatch short globalArgs [x] ys c
parseArgs ([]:xs)           c = parseArgs xs c
parseArgs (x:xs)            c = parseArgs xs (setInFile c x)

runLoop :: Stack -> Env -> IO ()
runLoop s e = do
      putStr "> "
      expr <- getLine >>= return . buildExpr . L.tokenise
      let (s',e') = eval expr s e
      print s'
      runLoop s' e'

loadFile :: Maybe FilePath -> IO String
loadFile (Just f) = readFile f
loadFile Nothing  = error "No input"

export :: Maybe FilePath -> String -> IO ()
export (Just f) s = writeFile f s
export Nothing  s = putStrLn s

main = do 
  hSetBuffering stdout NoBuffering
  args <- getArgs 
  if (args == ["--help"] || args == ["-h"]) then putStrLn $ printHelp globalArgs
  else do
    let settings = parseArgs args initialSettings
    if (mode settings == Interactive) then
      if (inFile settings == Nothing) then runLoop [] []
      else do
        file <- loadFile (inFile settings)
        let (stack,env) = eval (buildExpr $ L.tokenise file) [] []
        runLoop stack env
    else do
      file <- loadFile (inFile settings)
      out <- return file >>= return . buildExpr . L.tokenise >>= return . Y.genCode (opt settings)
      export (outputFile settings) out
