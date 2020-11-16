module JVM where
import AbsInstant
import Control.Monad.State
import Data.Map as M

-- SymTab, instructions, max local, max stack
type Memory = (M.Map Ident Integer, [String], Integer, Integer)
type MyMonad a = StateT Memory IO a


appendStringInstr :: [String] -> MyMonad ()
appendStringInstr s = do
    (vars, ins, ml, ms) <- get
    put $ (vars, s ++ ins, ml, ms)

myToInteger :: Bool -> Integer
myToInteger True = 1
myToInteger False = 0
        
compileToJVM :: Program -> String -> IO String
compileToJVM p moduleName = do
    (instructions, max_local, max_stack) <- evalStateT (compileP p) (M.empty, [], 1, 0)
    return $ ".class  public " ++ moduleName ++ "\n" ++
             ".super  java/lang/Object\n" ++
             "\n" ++
             ".method public static main([Ljava/lang/String;)V\n" ++
             ".limit locals " ++ show max_local ++ "\n" ++
             ".limit stack " ++ show max_stack ++ "\n" ++
             "\n" ++
             Prelude.foldr (\x y -> y ++ "    " ++ x ++ "\n") "" instructions ++ 
             "    return\n" ++ 
             "\n" ++
             ".end method\n"

compileP :: Program -> MyMonad ([String], Integer, Integer)
compileP (Prog []) = do
    (_, instructions, max_local, max_stack) <- get
    return (instructions, max_local, max_stack)
compileP (Prog (s:ss)) = do
    compileS s
    compileP $ Prog ss

compileS :: Stmt -> MyMonad ()
compileS s = case s of
    SAss i e -> do
        (newIns, stack) <- compileE e
        appendStringInstr newIns
        (symTab, ins, ml, ms) <- get
        if member i symTab
        then do
            put (symTab, ins, ml, max ms stack)
            appendStringInstr $ [storeVar $ symTab ! i]
        else do
            put (insert i ml symTab, ins, ml + 1, max ms stack)
            appendStringInstr $ [storeVar ml]
    SExp e -> do
        (newIns, stack) <- compileE e
        appendStringInstr newIns
        appendStringInstr ["getstatic java/lang/System/out Ljava/io/PrintStream;"] -- TODO Optimize stack
        appendStringInstr ["swap"]
        appendStringInstr ["invokevirtual java/io/PrintStream/println(I)V"]
        (symTab, ins, ml, ms) <- get
        put (symTab, ins, ml, max ms (max stack 2))

compileE :: Exp -> MyMonad ([String], Integer)
compileE e = case e of
    ExpAdd e1 e2 -> binaryOperation e1 e2 "iadd" True
    ExpSub e1 e2 -> binaryOperation e1 e2 "isub" False
    ExpMul e1 e2 -> binaryOperation e1 e2 "imul" True
    ExpDiv e1 e2 -> binaryOperation e1 e2 "idiv" False
    ExpLit i -> do return ([putConst i], 1)
    ExpVar i -> do
        (symTab, _, _, _) <- get
        return ([loadVar $ symTab ! i], 1)

binaryOperation :: Exp -> Exp -> String -> Bool -> MyMonad ([String], Integer)
binaryOperation e1 e2 s c = do
    (ins1, stack1) <- compileE e1
    (ins2, stack2) <- compileE e2
    let newOperation = s
    return (
        if stack1 == stack2 then
            (newOperation:(ins2 ++ ins1), stack1 + 1)
        else if stack1 > stack2 then
            (newOperation:(ins2 ++ ins1), stack1)
        else if c then
            (newOperation:"swap":(ins1 ++ ins2), stack2)
        else
            (newOperation:(ins1 ++ ins2), stack2))

putConst :: Integer -> String
putConst i
    | i <= 2        = "iconst_" ++ show i
    | i <= 127      = "bipush " ++ show i
    | i <= 32767    = "sipush " ++ show i
    | otherwise     = "ldc "     ++ show i

storeVar :: Integer -> String
storeVar i
    | i <= 3    = "istore_" ++ show i
    | otherwise = "istore " ++ show i

loadVar :: Integer -> String
loadVar i
    | i <= 3    = "iload_" ++ show i
    | otherwise = "iload " ++ show i
