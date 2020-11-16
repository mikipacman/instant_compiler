module Main where
import LLVM
import Parse
import System.Environment (getArgs)
import System.FilePath.Posix
import System.Process

main :: IO ()
main = do
    args <- getArgs

    case length args of
        1 -> do
            let filePath = args !! 0
            let fileName = dropExtension $ takeBaseName filePath
            let newFileName = replaceExtension filePath "ll"
            let newBCFileName = replaceExtension filePath "bc"

            abstractTree <- parseFile $ filePath
            programString <- compileToLLVM abstractTree fileName
            writeFile newFileName programString

            callProcess "llvm-as" [newFileName]
            callProcess "llvm-link" ["-o", newBCFileName, newBCFileName, "lib/runtime.bc"]

        otherwise -> putStrLn "Provide only one argument!"
