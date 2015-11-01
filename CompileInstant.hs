{-# LANGUAGE FlexibleContexts #-}

module CompileInstant where

import AbsInstant
import Data.Map as M
import Data.Set as S
import Control.Monad.State
import Data.Array
import Data.Char

type Loc = Integer
type Line = Integer 
type Doc = [ShowS] -> [ShowS]

type Env = (M.Map Ident Integer)
data Store = ST (Env, Line, Loc) deriving (Eq,Ord,Show)


-------------------------
-- Compiler structures --
-------------------------

mainMethod = ".method public static main([Ljava/lang/String;)V"
limitStack limit = ".limit stack " ++ (show limit)
limitLocals limit = ".limit locals " ++ (show limit)
line no = ".line " ++ (show no)
preparePrint = "getstatic java/lang/System/out Ljava/io/PrintStream;"
printLine ="invokevirtual java/io/PrintStream/println(I)V"
ret = "return"
end = ".end method\n"
printVar = 1

classDefinition className = ".class  public " ++ className
superDefinition = ".super java/lang/Object"

initMethod = ".method public <init>()V"
invokeInit = "invokespecial java/lang/Object/<init>()V"


load loc = "iload_" ++ (show loc)
store loc = "istore_" ++ (show loc)
constInt int = if int > 5 then "bipush " ++ (show int)
				else "iconst_" ++ (show int)

epilog = do
	outCnt ret
	out end

prolog program = do
	out $ classDefinition "Auto"
	out superDefinition
	out initMethod
	outIndent $ limitStack 1
	outIndent $ limitLocals 1
	outIndent $ line 1
	outCnt "aload_0"
	outCnt invokeInit
	epilog
	out mainMethod
	stack <- countStack program
	outIndent $ limitStack stack
	varCount <- countLocals program
	outIndent $ limitLocals (varCount + 2)

------------
-- Output --
------------

createFile = do
	lift $ writeFile "inst_jvm.j" "; instant compiler\n"

out string = do 
	lift $ appendFile "inst_jvm.j" (string ++ "\n")

outIndent string = do
	out $ "\t" ++ string

outCnt string = do
	ST (s, lineNo, n) <- get
	put $ ST (s, lineNo + 1, n)
	outIndent $ prepareLine lineNo string

prepareLine i line = (show i) ++ ": " ++ line


clearStore :: Env
clearStore = M.empty

initialStore :: Store
initialStore = ST (clearStore, 0, 2)

-------------------------------------
-- Environment and Store functions --
-------------------------------------

alloc ident = do
	ST (store, l, nextLoc) <- get
	case M.lookup ident store of
		(Just loc) -> return loc
		Nothing -> do
			put $ ST (M.insert ident nextLoc store, l, nextLoc + 1)
			return nextLoc

getLoc ident = do
	ST (store, _, _) <- get
	case M.lookup ident store of 
		(Just val) -> return val
		Nothing -> fail $ "Variable " ++ (show ident) ++ " is not defined.\n"


-----------
-- Utils --
-----------

countLocals (Prog stmts) = do
	locals <- foldM getLocals S.empty stmts
	return $ S.size locals

getLocals locals (SAss ident expr) = do
	return $ S.insert ident locals

getLocals locals _ = do
	return locals

countStack (Prog stmts) = do
	stack <- foldM getStack 0 stmts
	return stack

getStack stack (SAss _ expr) = do
	newStack <- getStackE expr
	return $ max stack newStack

getStack stack (SExp expr) = do
	newStack <- getStackE expr
	return $ max stack newStack

getStackE (ExpAdd e1 e2) = do
	s1 <- getStackE e1
	s2 <- getStackE e2
	return $ max (s1 + 1) (s2 + 1)

getStackE (ExpSub e1 e2) = do
	s1 <- getStackE e1
	s2 <- getStackE e2
	return $ max (s1 + 1) (s2 + 1)

getStackE (ExpMul e1 e2) = do
	s1 <- getStackE e1
	s2 <- getStackE e2
	return $ max (s1 + 1) (s2 + 1)

getStackE (ExpDiv e1 e2) = do
	s1 <- getStackE e1
	s2 <- getStackE e2
	return $ max (s1 + 1) (s2 + 1)

getStackE _ = do
	return 0

-------------
-- Program --
-------------

class Compile a where
	compile :: a -> String


compile' p = evalStateT (compileProg p) initialStore

-- instance Compile Program where
-- 	compile ast = case ast of
-- 		Prog stmts -> 

compileProg prog@(Prog stmts) = do
	createFile
	prolog prog
	lineCnt <- foldM compileStmt 2 stmts
	epilog
	return ()


----------------
-- Statements --
----------------

-- var = expr
compileStmt inLine (SAss ident expr) = do
	outIndent $ line inLine
	compileExpr expr
	loc <- alloc ident
	outCnt $ store loc
	return $ inLine + 1

-- var = expr
compileStmt inLine (SExp expr) = do
	-- expr <- optimize expr
	outIndent $ line inLine
	compileExpr expr
	outCnt $ store printVar
	outCnt preparePrint
	outCnt $ load printVar
	outCnt printLine
	return $ inLine + 1


-----------------
-- Expressions --
-----------------

-- (a + b) + c != a + (b + c)

compileExpr (ExpAdd expr1 expr2) = do
	compileExpr expr2
	compileExpr expr1
	outCnt "iadd"

compileExpr (ExpSub expr1 expr2) = do
	compileExpr expr1
	compileExpr expr2
	outCnt "isub"

compileExpr (ExpMul expr1 expr2) = do
	compileExpr expr1
	compileExpr expr2
	outCnt "imul"

compileExpr (ExpDiv expr1 expr2) = do
	compileExpr expr1
	compileExpr expr2
	outCnt "idiv"

compileExpr (ExpLit integer) = do
	outCnt $ constInt integer 

compileExpr (ExpVar ident) = do
	loc <- getLoc ident
	outCnt $ load loc

--------------
-- Optimize --
--------------

optimize s = do
	return s