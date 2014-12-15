{-# LANGUAGE MultiParamTypeClasses #-}
module While.Concrete where

import Abstat.Interface.State hiding (State)
import Abstat.Interface.Semantics
import Abstat.Common.ConcreteState
import While.AST

type Domain = Integer

interpretAExpr :: AExpr -> State Domain -> Domain
interpretAExpr expr state = case expr of
    (Var var) -> load var state
    (IntConst num) -> num
    (AUnary Neg a) ->         negate (interpretAExpr a state)
    (ABinary Add a b) ->      interpretAExpr a state + interpretAExpr b state
    (ABinary Subtract a b) -> interpretAExpr a state - interpretAExpr b state
    (ABinary Multiply a b) -> interpretAExpr a state * interpretAExpr b state
    (ABinary Divide a b) ->   interpretAExpr a state `quot` interpretAExpr b state

-- Postcondition: the root node of the resulting BExpr
-- is one operation from Not, And, Less, Equal
reduceBExpr :: BExpr -> BExpr
reduceBExpr expr = case expr of
    (BBinary Or a b) ->
        BUnary Not $ BBinary And (BUnary Not a) (BUnary Not b)
    (RBinary NotEqual a b) ->
        BUnary Not $ RBinary Equal a b
    (RBinary GreaterEqual a b) ->
        BUnary Not $ RBinary Less a b
    (RBinary Greater a b) ->
        BBinary And
            (BUnary Not $ RBinary Less a b)
            (BUnary Not $ RBinary Equal a b)
    (RBinary LessEqual a b) ->
        BUnary Not $ BBinary And
            (BUnary Not $ RBinary Less a b)
            (BUnary Not $ RBinary Equal a b)
    _ -> expr

interpretBExpr :: BExpr -> State Domain -> Bool
interpretBExpr expr state = case expr of
    (BoolConst val) -> val
    (BUnary Not b) -> not (interpretBExpr b state)
    (BBinary And a b)
        | interpretBExpr a state -> interpretBExpr b state
        | interpretBExpr b state -> False
        | otherwise -> False
    BBinary{} -> interpretBExpr (reduceBExpr expr) state
    (RBinary Equal a b) -> interpretAExpr a state == interpretAExpr b state
    (RBinary Less a b) ->  interpretAExpr a state < interpretAExpr b state
    RBinary{} -> interpretBExpr (reduceBExpr expr) state

interpretStmt :: Stmt -> State Domain -> State Domain
interpretStmt stmt state = case stmt of
    Skip ->
        state

    (Assign var aexpr) ->
        store var val state
        where val = interpretAExpr aexpr state

    (Seq stmt1 stmt2) ->
        interpretStmt stmt2 $ interpretStmt stmt1 state

    (If test thenStmt elseStmt) ->
        if interpretBExpr test state
        then interpretStmt thenStmt state
        else interpretStmt elseStmt state

    (While test bodyStmt) ->
        if interpretBExpr test state
        then interpretStmt stmt $ interpretStmt bodyStmt state
        else state

instance Semantics Stmt State Integer where
    interpretState = interpretStmt
