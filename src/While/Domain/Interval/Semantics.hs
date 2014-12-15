{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module While.Domain.Interval.Semantics where

import Abstat.Interface.GaloisConnection
import Abstat.Interface.State
import Abstat.Interface.Lattice
import Abstat.Interface.AbstractDomain()
import While.AST
import While.AbstractSemantics
import While.Domain.Interval.Domain

instance WhileAbstractSemantics Domain where
    interpretAExpr expr state = case expr of
        (Var var) -> load var state

        (IntConst num) -> singleAbstraction num

        (AUnary op x) -> case op of
            Neg -> case interpretAExpr x state of
                Bottom -> Bottom
                (Interval a b) -> Interval (negate b) (negate a)

        (ABinary op x y) -> case op of
            Add -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Interval a b, Interval c d) -> Interval (a+c) (b+d)
            
            Subtract -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Interval a b, Interval c d) -> Interval (a-d) (b-c)
            
            Multiply -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Interval a b, Interval c d) ->
                    join
                        (Interval (min (a*c) (b*c)) (max (a*c) (b*c)))
                        (Interval (min (a*d) (b*d)) (max (a*d) (b*d)))

            Divide -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> Bottom
                (Bottom, _) -> Bottom
                (Interval a b, Interval c d) ->
                    case splitAtZero (Interval c d) of
                        (Bottom, Bottom) -> Bottom
                        (Interval e f, Bottom) ->
                            [a, b] `divide` [e, f]
                        (Bottom, Interval g h) ->
                            [a, b] `divide` [g, h]
                        (Interval e f, Interval g h) ->
                            [a, b] `divide` [e, f, g, h]
                    where
                        divide :: [Value] -> [Value] -> Domain
                        divide as bs = foldr (\aa -> join (divide' aa bs)) Bottom as
                        divide' :: Value -> [Value] -> Domain
                        divide' aa = foldr (join . divide'' aa) Bottom
                        divide'' :: Value -> Value -> Domain
                        divide'' (Val aa) (Val bb) =
                            Interval (Val $ aa `quot` bb) (Val $ aa `quot` bb)
                        divide'' PlusInf PlusInf = Interval (Val 0) PlusInf
                        divide'' PlusInf MinusInf = Interval MinusInf (Val 0)
                        divide'' MinusInf PlusInf = Interval MinusInf (Val 0)
                        divide'' MinusInf MinusInf = Interval (Val 0) PlusInf
                        divide'' PlusInf (Val 0) = error "PlusInf / 0"
                        divide'' PlusInf (Val bb) = 
                            if bb < 0
                            then Interval MinusInf MinusInf
                            else Interval PlusInf PlusInf
                        divide'' MinusInf (Val 0) = error "MinusInf / 0"
                        divide'' MinusInf (Val bb) =
                            if bb < 0
                            then Interval PlusInf PlusInf
                            else Interval MinusInf MinusInf
                        divide'' (Val _) PlusInf = Interval (Val 0) (Val 0)
                        divide'' (Val _) MinusInf = Interval (Val 0) (Val 0)

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

        (BBinary op x y) -> case op of
            And -> meet (interpretBExpr x state) (interpretBExpr y state)
            Or ->  join (interpretBExpr x state) (interpretBExpr y state)

        (RBinary op x y) -> case op of
            Equal -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Interval a b, Interval c d)
                    | max a c <= min b d -> state
                    | otherwise -> bottom

            NotEqual -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Interval a b, Interval c d)
                    | a == b && b == c && c == d -> bottom
                    | otherwise -> state

            Less -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Interval a _, Interval _ d)
                    | a >= d -> bottom
                    | otherwise -> state

            LessEqual -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Interval a _, Interval _ d)
                    | a > d -> bottom
                    | otherwise -> state

            Greater -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Interval _ b, Interval c _)
                    | b <= c -> bottom
                    | otherwise -> state

            GreaterEqual -> case (interpretAExpr x state, interpretAExpr y state) of
                (_, Bottom) -> bottom
                (Bottom, _) -> bottom
                (Interval _ b, Interval c _)
                    | b < c -> bottom
                    | otherwise -> state
