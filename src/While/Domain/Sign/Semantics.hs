{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module While.Domain.Sign.Semantics where

import Abstat.Interface.GaloisConnection
import Abstat.Interface.State
import Abstat.Interface.Lattice
import Abstat.Interface.AbstractDomain()
import Abstat.Common.FlatDomain
import While.AST
import While.AbstractSemantics
import While.Domain.Sign.Domain

instance WhileAbstractSemantics Domain where
    interpretAExpr expr state = case expr of
        (Var var) -> load var state

        (IntConst num) -> singleAbstraction num

        (AUnary op x) -> case op of
            Neg -> case interpretAExpr x state of
                Bottom -> Bottom
                (Val Positive) -> Val Negative
                (Val Negative) -> Val Positive
                (Val Zero) -> Val Zero
                _ -> Top

        (ABinary op a b) -> case op of
            Add -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Val Zero, Val x) -> Val x
                (Val x, Val Zero) -> Val x
                (Val Positive, Val Positive) -> Val Positive
                (Val Negative, Val Negative) -> Val Negative
                (Val Positive, Val Negative) -> Top
                (Val Negative, Val Positive) -> Top
                (_, _) -> Top
            
            Subtract -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Val x, Val Zero) -> Val x
                (Val Zero, Val Negative) -> Val Positive
                (Val Zero, Val Positive) -> Val Negative
                (Val Positive, Val Negative) -> Val Positive
                (Val Negative, Val Positive) -> Val Negative
                (Val Positive, Val Positive) -> Top
                (Val Negative, Val Negative) -> Top
                (_, _) -> Top
            
            Multiply -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Val Zero, Top) -> Val Zero
                (Val Zero, Val _) -> Val Zero
                (Top, Val Zero) -> Val Zero
                (Val _, Val Zero) -> Val Zero
                (Val Positive, Val Positive) -> Val Positive
                (Val Positive, Val Negative) -> Val Negative
                (Val Negative, Val Negative) -> Val Positive
                (Val Negative, Val Positive) -> Val Negative
                (_, _) -> Top

            Divide -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (_, Val Zero) -> Bottom
                (Val Zero, Val _) -> Val Zero
                (Val Positive, Val Positive) -> Top
                (Val Positive, Val Negative) -> Top
                (Val Negative, Val Negative) -> Top
                (Val Negative, Val Positive) -> Top
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
            Equal -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Val Zero, Val Zero) -> state
                (Val Positive, Val Positive) -> state
                (Val Negative, Val Negative) -> state
                (Val _, Val _) -> bottom
                (_, _) -> state 

            NotEqual -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Val Zero, Val Zero) -> bottom
                (_, _) -> state

            Less -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Val Zero, Val Negative) -> bottom
                (Val Zero, Val Zero) -> bottom
                (Val Positive, Val Negative) -> bottom
                (Val Positive, Val Zero) -> bottom
                (_, _) -> state

            LessEqual -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Val Zero, Val Negative) -> bottom
                (Val Positive, Val Negative) -> bottom
                (Val Positive, Val Zero) -> bottom
                (_, _) -> state

            Greater -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Val Negative, Val Zero) -> bottom
                (Val Negative, Val Positive) -> bottom
                (Val Zero, Val Zero) -> bottom
                (Val Zero, Val Positive) -> bottom
                (_, _) -> state

            GreaterEqual -> case (interpretAExpr a state, interpretAExpr b state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Val Negative, Val Zero) -> bottom
                (Val Negative, Val Positive) -> bottom
                (Val Zero, Val Positive) -> bottom
                (_, _) -> state
