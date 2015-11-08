module LLVM where

import AbsInstant
import Data.Map as M
import Data.Set as S
import Control.Monad.State
import Data.Array
import Data.Char


type Reg = Integer
type Doc = [ShowS] -> [ShowS]

data RegVal = Var Ident | Lit Integer | Rg Reg deriving (Eq,Ord)

instance Show RegVal where
	show (Var (Ident ident)) = "%" ++ id ident
	show (Lit value) = show value
	show (Rg reg) = "%" ++ show reg

type Env = S.Set Ident
data Store = ST (Env, Reg) deriving (Eq, Ord, Show)

----------------
-- Store LLVM --
----------------

clearStore :: Store
clearStore = ST (clearEnv, 1)

clearEnv :: Env
clearEnv = S.empty

alloc ident = do
	ST (env, r) <- get
	case S.member ident env of
		True -> return (Var ident)
		False -> do
			put $ ST (S.insert ident env, r)
			printAlloca (Var ident)
			return (Var ident)

load ident = do
	ST (env, r) <- get
	case S.member ident env of
		True -> do
			reg <- getNextReg
			printLoad (Var ident) reg
			return (Rg reg)
		False -> fail $ "Variable " ++ (show ident) ++ " is not defined.\n"

store ident value = do
	printStore (Var ident) value

getNextReg = do
	ST (e, reg) <- get
	put $ ST (e, reg + 1)
	return reg

-----------------
-- Output LLVM --
-----------------

printAlloca ident = do
	printIndent $ (show ident) ++ " = alloca i32, align 4"

printStore ident value = do
	printIndent $ "store i32 " ++ (show value) ++ ", i32* " ++ (show ident) ++ ", align 4"

printLoad ident reg = do
	printIndent $ "%" ++ (show reg) ++ " = load i32* " ++ (show ident) ++ ", align 4"

printIndent string = do
	printLine $ "\t" ++ string

printLine string = do
	lift $ appendFile "inst_llvm.ll" $ string ++ "\n"

llvm action val1 val2 = do
	nextReg <- getNextReg
	printIndent $ "%" ++ (show nextReg) ++ " = " ++ action ++ " nsw i32 " ++ (show val1) ++ ", " ++ (show val2)
	return (Rg nextReg)

printVal reg = do
	printIndent $ "call void @printInt(i32 " ++ (show reg) ++ ")"

------------------
-- Program LLVM --
------------------

compile p = evalStateT (compileProg p) clearStore

prolog = do
	printLine $ "declare void @printInt(i32)"
	printLine $ "define i32 @main() {"

epilog = do
	printIndent $ "ret i32 0"
	printLine $ "}"

compileProg (Prog stmts) = do
	prolog
	forM_ stmts compileStmt
	epilog
	return ()


---------------------
-- Statements LLVM --
---------------------

compileStmt (SAss ident expr) = do
	alloc ident
	exprLoc <- compileExpr expr
	store ident exprLoc

compileStmt (SExp expr) = do
	exprLoc <- compileExpr expr
	printVal exprLoc

---------------------
-- Expresions LLVM --
---------------------

compileExpr (ExpAdd expr1 expr2) = do
	val1 <- compileExpr expr1
	val2 <- compileExpr expr2
	newReg <- llvm "add" val1 val2
	return newReg

compileExpr (ExpMul expr1 expr2) = do
	val1 <- compileExpr expr1
	val2 <- compileExpr expr2
	newReg <- llvm "mul" val1 val2
	return newReg

compileExpr (ExpSub expr1 expr2) = do
	val1 <- compileExpr expr1
	val2 <- compileExpr expr2
	newReg <- llvm "sub" val1 val2
	return newReg

compileExpr (ExpDiv expr1 expr2) = do
	val1 <- compileExpr expr1
	val2 <- compileExpr expr2
	newReg <- llvm "div" val1 val2
	return newReg

compileExpr (ExpLit value) = do
	return (Lit value)

compileExpr (ExpVar ident) = do
	newReg <- load ident
	return newReg