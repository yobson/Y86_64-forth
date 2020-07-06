{-# LANGUAGE ScopedTypeVariables, DataKinds #-}

module Main where

import System.IO
import System.Environment (getArgs)
import System.Exit (exitSuccess)
import Control.Exception (AsyncException(..), catch, throw)
import System.Process (callProcess, readProcess)
import Control.Monad.State.Lazy (runState)

import qualified Compiler.Lex    as L
import           Compiler.Parse
import qualified Compiler.Y86    as Y
import           Compiler.Y86.Optimiser (transformForth)
import           Interpreter


type HelpText = String

data Argument = Flag String | Input String | Switch Char | Group Char Argument

data Plan = Interactive | Compile | Dump DumpOpt deriving Eq

data DumpOpt = Forth | Forth2 deriving Eq

data CompileSettings = CompSet { mode       :: Plan
                               , outputFile :: Maybe FilePath
                               , inFile     :: Maybe [FilePath]
                               , opt        :: Bool
                               , rlwrap     :: Bool
                               , basePtr    :: Int
                               }

type Arg = (Argument, HelpText, CompileSettings -> String -> CompileSettings)

initialSettings = CompSet { mode = Compile
                          , outputFile = Nothing
                          , inFile     = Nothing
                          , opt        = True
                          , rlwrap     = True
                          , basePtr    = 0x180
                          }

globalArgs = [ (Flag "interactive", "Run Repl", setRepl)
             , (Input "output", "Set output file name", setOutFile)
             , (Flag "no-opt", "Turn off optimisation", turnOffOpt)
             , (Input "base-pointer", "Manually set base pointer initial address", setBasePtr)
             , (Flag "rlwrap-off", "Turn off rlwrap", turnOffRL)
             , (Group 'd' (Flag "dump-forth"), "Dump Forth AST", setDump Forth)
             , (Group 'd' (Flag "dump-forth-trans"), "Dump Forth AST after code transformation", setDump Forth2)
             ]


turnOffOpt, setOutFile, setInFile, setRepl, 
  turnOffRL, setBasePtr :: CompileSettings -> String -> CompileSettings
turnOffOpt s _ = s {opt        = False}
setOutFile s f = s {outputFile = Just f}
setRepl    s _ = s {mode       = Interactive}
turnOffRL  s _ = s {rlwrap     = False}
setBasePtr s f = s {basePtr    = read f}
setDump d  s _ = s {mode       = Dump d}
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
short ((Group _ _),_,_)    = []

long :: Arg -> String
long ((Flag   s),_,_)  = s
long ((Input  s),_,_)  = s
long ((Switch _),_,_)  = ""
long ((Group c a),_,_) = c:(long (a, undefined, undefined))

match :: Arg -> [String] -> CompileSettings -> CompileSettings
match (Flag  i, _, f) ys     c = parseArgs ys $ f c ""
match (Input s, _, f) (y:ys) c = parseArgs ys $ f c y
match (Switch s,_, f) ys     c = parseArgs ys $ f c ""
match (Group _ _,_,f) ys     c = parseArgs ys $ f c ""

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
    case mode settings of
      (Interactive) ->  do
        if (rlwrap settings) then 
          callProcess "rlwrap" $ "./FORTH":"-r":args
        else 
          if (inFile settings == Nothing) then runLoop [] [] []
          else do
            files <- loadFiles (inFile settings)
            let ((stack,env),m) = runState (foldEval files [] []) []
            runLoop stack env m
      (Compile) ->  do
        files <- loadFiles (inFile settings)
        out <- return files >>= return . concatExpr . map (buildExpr . L.tokenise) >>= return . Y.genCode (opt settings) (basePtr settings)
        export (outputFile settings) out
      (Dump Forth) -> do 
        files <- loadFiles (inFile settings)
        out <- return files >>= return . concatExpr . map (buildExpr . L.tokenise) >>= return . show
        export (outputFile settings) out
      (Dump Forth2) -> do 
        files <- loadFiles (inFile settings)
        out <- return files >>= return . concatExpr . map (buildExpr . L.tokenise)
        out2 <- return (transformForth out) >>= return . show
        putStrLn "Forth AST after transformation"
        export (outputFile settings) out2
