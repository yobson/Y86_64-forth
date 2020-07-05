{-# LANGUAGE ScopedTypeVariables, DataKinds #-}

module Main (Expr, main) where

import System.IO
import qualified Lex as L
import Parse
import qualified Y86 as Y
import System.Environment
import Control.Monad.State.Lazy
import System.Exit
import Control.Exception
import System.Process 

type Ident = String


type Stack = [Int]
type Env   = [(Ident, Expr)]
type Mem   = [(Ident, Int)]

type M a = State Mem a


instance Semigroup (Expr) where
  (<>) (x :> xs) e = x :> (xs <> e)
  (<>)  x        e = x :> e

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
eval (Nop)                 xs  env = return $ (xs,env)
eval (Number i)            xs  env = return $ (i:xs,env)
eval cl@(Closure i args e) xs  env = return $ (xs, elab env i cl)
eval (Prim i)              xs  env = return $ (primEval i xs, env)
eval (If e1 e2)         (0:xs) env = eval e1 xs env
eval (If e1 e2)         (_:xs) env = eval e2 xs env
eval (e1 :> e2)            xs  env = eval e1 xs env >>= uncurry (eval e2)
eval (Deref i)             xs  env = findM i >>= \x -> return (x:xs, env)
eval (Set i)            (x:xs) env = set i x >> return (xs,env)
eval (Variable i)          xs  env = case (find i env) of
                                       (Closure i args expr) -> do 
                                              let (env',s) = rename args xs env
                                              (s',_)       <- eval expr s env' 
                                              return (s', env)
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
                               , inFile     :: Maybe [FilePath]
                               , opt        :: Bool
                               , rlwrap     :: Bool
                               }
                            deriving (Show)

type Arg = (Argument, HelpText, CompileSettings -> String -> CompileSettings)

initialSettings = CompSet Compile Nothing Nothing True True

turnOffOpt, setOutFile, setInFile, setRepl :: CompileSettings -> String -> CompileSettings
turnOffOpt s _ = s {opt        = False}
setOutFile s f = s {outputFile = Just f}
setRepl    s _ = s {mode       = Interactive}
turnOffRL  s _ = s {rlwrap     = False}
setInFile  s f = case inFile s of
                    (Nothing) -> s {inFile = Just [f]}
                    (Just xs) -> s {inFile = Just (xs ++ [f])}

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
             , (Flag "opt-off", "Turn off optimisation", turnOffOpt)
             , (Flag "rlwrap-off", "Turn off rlwrap", turnOffRL)
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

printMem' :: Mem -> IO ()
printMem' []         = putStrLn "[]"
printMem' [(i,v)]    = putStr i >> putStr " := " >> putStr (show v) >> putStrLn "]"
printMem' ((i,v):xs) = putStr i >> putStr " := " >> putStr (show v) >> putStr ", " >> printMem' xs

printMem xs = putStr "[" >> printMem' xs

exitS UserInterrupt = exitSuccess
exitS e = throw e

runLoop :: Stack -> Env -> Mem -> IO ()
runLoop s e mem = flip catch exitS $ do
         { putStr "> "
         ; expr <- getLine >>= return . buildExpr . L.tokenise
         ; let ((s',e'), m) = runState (eval expr s e) mem
         ; putStr "Stack: " >> print s'
         ; putStr $ if (length s' < 2) then "" else "        ^\n"
         ; if (not $ null m) then putStr "VARs: " >> printMem m >> runLoop s' e' m
         ; else runLoop s' e' m
         }

loadFiles :: Maybe [FilePath] -> IO [String]
loadFiles (Just f) = mapM (\x -> readProcess "cpp" ["-P", "-w", x] []) f
loadFiles Nothing  = error "No input"

export :: Maybe FilePath -> String -> IO ()
export (Just f) s = writeFile f s
export Nothing  s = putStrLn s

eval' ex s e = eval (buildExpr $ L.tokenise ex) s e

foldEval :: [String] -> Stack -> Env -> M (Stack, Env)
foldEval []     s e = error "No input"
foldEval [x]    s e = eval' x s e
foldEval (x:xs) s e = eval' x s e >>= uncurry (foldEval xs)

concatExpr :: [Expr] -> Expr
concatExpr [] = error "Empty Expression"
concatExpr [x] = x
concatExpr (x:xs) = x <> (concatExpr xs)

main = do 
  hSetBuffering stdout NoBuffering
  hSetBuffering stdin LineBuffering
  args <- getArgs 
  if (args == ["--help"] || args == ["-h"]) then putStrLn $ printHelp globalArgs
  else do
    let settings = parseArgs args initialSettings
    if (mode settings == Interactive) then do
      if (rlwrap settings) then 
        callProcess "rlwrap" $ "./FORTH":"-r":args
      else 
        if (inFile settings == Nothing) then runLoop [] [] []
        else do
          files <- loadFiles (inFile settings)
          let ((stack,env),m) = runState (foldEval files [] []) []
          runLoop stack env m
      else do
        files <- loadFiles (inFile settings)
        out <- return files >>= return . concatExpr . map (buildExpr . L.tokenise) >>= return . Y.genCode (opt settings)
        export (outputFile settings) out
