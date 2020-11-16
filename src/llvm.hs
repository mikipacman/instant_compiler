module LLVM where
import AbsInstant
import Control.Monad.State
import Data.Map as M


-- SymTab, MaxExprID
type Memory = (M.Map Ident Integer, Integer)
data ExpName = Id Integer | Value Integer 
type MyMonad a = StateT Memory IO a


compileToLLVM :: Program -> String -> IO String
compileToLLVM p moduleName = do
    instructions <- evalStateT (compileP p) (M.empty, 0)
    return $ "declare void @printInt(i32)\n" ++
             "define i32 @main() {\n" ++
             Prelude.foldl(\x y -> x ++ "    " ++ y ++ "\n") "" instructions ++ 
             "    ret i32 0\n" ++
             "}\n"

compileP :: Program -> MyMonad [String]
compileP (Prog []) = return []
compileP (Prog (s:ss)) = do
    ins <- compileS s 
    ins2 <- compileP $ Prog ss
    return $ ins ++ ins2

compileS :: Stmt -> MyMonad [String]
compileS s = case s of
    SAss i e -> do
        (newIns, expName) <- compileE e
        (symTab, maxExpId) <- get
        put (insert i (maxExpId + 1) symTab, maxExpId + 1)
        return $ newIns ++ [(expNameToString $ Id $ maxExpId + 1) ++ " = add i32 0, " ++ expNameToString expName]
    SExp e -> do
        (newIns, expName) <- compileE e
        return $ newIns ++ ["call void @printInt(i32 " ++ expNameToString expName ++ ")"]

compileE :: Exp -> MyMonad ([String], ExpName)
compileE e = case e of
    ExpAdd e1 e2 -> binaryOperation e1 e2 "add" 
    ExpSub e1 e2 -> binaryOperation e1 e2 "sub" 
    ExpMul e1 e2 -> binaryOperation e1 e2 "mul"
    ExpDiv e1 e2 -> binaryOperation e1 e2 "udiv"
    ExpLit i -> do 
        return ([], Value i)
    ExpVar i -> do
        (symTab, _) <- get
        return ([], Id $ symTab ! i)

binaryOperation :: Exp -> Exp -> String -> MyMonad ([String], ExpName)
binaryOperation e1 e2 s = do
    (ins1, expName1) <- compileE e1
    (ins2, expName2) <- compileE e2
    (symTab, maxExpId) <- get
    put (symTab, maxExpId + 1)
    return (ins1 ++ ins2 ++ [(expNameToString $ Id $ maxExpId + 1) ++ " = " ++ s ++ " i32 " ++ 
             (expNameToString expName1) ++ ", " ++ 
             (expNameToString expName2)], Id $ maxExpId + 1)

expNameToString :: ExpName -> String
expNameToString (Id i) = "%" ++ show i
expNameToString (Value i) = show i
