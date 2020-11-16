module Parse where

import System.IO (stdin, hGetContents)
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)

import LexInstant   (Token)
import ParInstant   (pProgram, myLexer)
import SkelInstant  ()
import PrintInstant (Print, printTree)
import AbsInstant   (Program)

type Err = Either String
type ParseFun a = [Token] -> Err a

myLLexer = myLexer

parseFile :: FilePath -> IO Program
parseFile file = readFile file >>= run pProgram

run :: ParseFun Program -> String -> IO Program
run p s = let ts = myLLexer s in case p ts of
    Left s -> do 
        putStrLn "\nParse Failed...\n"
        putStrLn s
        exitFailure
    Right tree -> return tree
