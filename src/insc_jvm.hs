module Main where
import JVM
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
            let newFileName = replaceExtension filePath "j"
            let newFileDir = takeDirectory filePath

            abstractTree <- parseFile $ filePath
            programString <- compileToJVM abstractTree fileName
            writeFile newFileName programString

            callProcess "java" ["-jar", "lib/jasmin.jar", newFileName, "-d", newFileDir]
        
        otherwise -> putStrLn "Provide only one argument!"
