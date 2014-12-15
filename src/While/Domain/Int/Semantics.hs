{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module While.Domain.Int.Semantics where

import Abstat.Interface.GaloisConnection
import Abstat.Interface.State
import Abstat.Interface.Lattice
import Abstat.Interface.AbstractDomain()
import While.AST
import While.AbstractSemantics
import While.Domain.Int.Domain

instance WhileAbstractSemantics Domain where
    interpretAExpr expr state = case expr of
        (Var var) -> load var state

        (IntConst num) -> singleAbstraction num

        (AUnary op x) -> case op of
            Neg -> case interpretAExpr x state of
                Bottom -> Bottom
                (Val n) -> Val (-n)
                _ -> Top

        (ABinary op a b) -> case op of
            Add -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Val x, Val y) -> Val $ x + y
                (_, _) -> Top
            
            Subtract -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Val x, Val y) -> Val $ x - y
                (_, _) -> Top
            
            Multiply -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Val 0, Top) -> Val 0
                (Top, Val 0) -> Val 0
                (Val x, Val y) -> Val $ x * y
                (_, _) -> Top

            Divide -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (_, Val 0) -> Bottom
                (Val 0, Top) -> Val 0
                (Val x, Val y) -> Val $ x `quot` y
                (_, _) -> Top

    interpretBExpr expr state = case expr of
        (BoolConst True) -> state

        (BoolConst False) -> bottom

        (BUnary op x) -> case op of
            Not -> interpretBExpr notExpr state where
                notExpr = case x of
                    (BoolConst a) -> BoolConst $ not a
                    (BUnary Not a) -> a
                    (BBinary And a b) -> BBinary Or (BUnary Not a) (BUnary Not b)
                    (BBinary Or a b) -> BBinary And (BUnary Not a) (BUnary Not b)
                    (RBinary NotEqual a b) -> RBinary Equal a b
                    (RBinary Equal a b) -> RBinary NotEqual a b
                    (RBinary GreaterEqual a b) -> RBinary Less a b
                    (RBinary Greater a b) -> RBinary LessEqual a b
                    (RBinary LessEqual a b) -> RBinary Greater a b
                    (RBinary Less a b) -> RBinary GreaterEqual a b

        (BBinary op a b) -> case op of
            And -> meet (interpretBExpr a state) (interpretBExpr b state)
            Or ->  join (interpretBExpr a state) (interpretBExpr b state)

        (RBinary op a b) -> case op of
            Equal -> case (a, b, interpretAExpr a state, interpretAExpr b state) of
                (_, _, Bottom, _) -> bottom
                (_, _, _, Bottom) -> bottom
                (Var var, _, _, Val y) -> store var (Val y) top `meet` state
                (_, Var var, Val x, _) -> store var (Val x) top `meet` state
                (_, _, Val x, Val y) ->
                    if x == y
                    then state
                    else bottom
                (_, _, _, _) -> state

            NotEqual -> case (interpretAExpr a state, interpretAExpr b state) of
                (Bottom, _) -> bottom
                (_, Bottom) -> bottom
                (Val x, Val y) ->
                    if x /= y
                    then state
                    else bottom
                (_, _) -> state

            Less -> case (interpretAExpr a state, interpretAExpr b state) of
                (Bottom, _) -> bottom
                (_, Bottom) -> bottom
                (Val x, Val y) ->
                    if x < y
                    then state
                    else bottom
                (_, _) -> state

            LessEqual -> case (interpretAExpr a state, interpretAExpr b state) of
                (Bottom, _) -> bottom
                (_, Bottom) -> bottom
                (Val x, Val y) ->
                    if x <= y
                    then state
                    else bottom
                (_, _) -> state

            Greater -> case (interpretAExpr a state, interpretAExpr b state) of
                (Bottom, _) -> bottom
                (_, Bottom) -> bottom
                (Val x, Val y) ->
                    if x > y
                    then state
                    else bottom
                (_, _) -> state

            GreaterEqual -> case (interpretAExpr a state, interpretAExpr b state) of
                (Bottom, _) -> bottom
                (_, Bottom) -> bottom
                (Val x, Val y) ->
                    if x >= y
                    then state
                    else bottom
                (_, _) -> state
