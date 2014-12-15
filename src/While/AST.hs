module While.AST where

import qualified Data.Set as Set

import Abstat.Interface.AST

-- Arithmetic expression
data AExpr
    = Var String
    | IntConst Integer
    | AUnary AUnaOp AExpr
    | ABinary ABinOp AExpr AExpr
    deriving (Eq,Show)

-- Arithmetic unary operations
data AUnaOp
    = Neg
    deriving (Eq,Show)

-- Arithmetic binary operations
data ABinOp
    = Add
    | Subtract
    | Multiply
    | Divide
    deriving (Eq,Show)

-- Boolean expressions
data BExpr
    = BoolConst Bool
    | BUnary BUnaOp BExpr
    | BBinary BBinOp BExpr BExpr
    | RBinary RBinOp AExpr AExpr
    deriving (Eq,Show)

-- Boolean unary operations
data BUnaOp
    = Not
    deriving (Eq,Show)

-- Boolean binary operations
data BBinOp
    = And
    | Or
    deriving (Eq,Show)

-- Relation binary operations
data RBinOp
    = Equal
    | NotEqual
    | Greater
    | GreaterEqual
    | Less
    | LessEqual
    deriving (Eq,Show)

data Stmt
    = Seq Stmt Stmt
    | Assign String AExpr
    | If BExpr Stmt Stmt
    | While BExpr Stmt
    | Skip
    deriving (Eq,Show)

instance AST AExpr where
    usedVars (Var var) = Set.singleton var
    usedVars (IntConst _) = Set.empty
    usedVars (AUnary _ a) = usedVars a
    usedVars (ABinary _ a b) = Set.union (usedVars a) (usedVars b)

instance AST BExpr where
    usedVars (BoolConst _) = Set.empty
    usedVars (BUnary _ a) = usedVars a
    usedVars (BBinary _ a b) = Set.union (usedVars a) (usedVars b)
    usedVars (RBinary _ a b) = Set.union (usedVars a) (usedVars b)

instance AST Stmt where
    usedVars (Seq a b) = Set.union (usedVars a) (usedVars b)
    usedVars (Assign var a) = Set.union (Set.singleton var) (usedVars a)
    usedVars (If test a b) = Set.unions [usedVars test, usedVars a, usedVars b]
    usedVars (While test a) = Set.union (usedVars test) (usedVars a)
    usedVars (Skip) = Set.empty
