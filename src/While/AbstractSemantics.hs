{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances #-}
module While.AbstractSemantics where

import Abstat.Interface.AbstractDomain
import Abstat.Interface.State hiding (State)
import Abstat.Interface.Semantics
import Abstat.Interface.Lattice
import Abstat.Interface.GaloisConnection
import Abstat.Interface.AbstractSemantics
import Abstat.Common.AbstractState
import While.AST

class (
        AbstractDomain abstract,
        GaloisConnection Integer abstract
    ) => WhileAbstractSemantics abstract where

    interpretAExpr :: AExpr -> State abstract -> abstract
    interpretBExpr :: BExpr -> State abstract -> State abstract

instance (
        Ord abstract,
        WhileAbstractSemantics abstract,
        Show abstract
    ) => Semantics Stmt State abstract where

    interpretState stmt state = case stmt of
        Skip ->
            state

        (Assign var aexpr) ->
            store var val state
            where val = interpretAExpr aexpr state

        (Seq stmt1 stmt2) ->
            interpretState stmt2 $ interpretState stmt1 state

        (If test thenStmt elseStmt) ->
            join afterThenStmt afterElseStmt
            where
                beforeThenStmt = interpretBExpr test state
                afterThenStmt = interpretState thenStmt beforeThenStmt
                beforeElseStmt = interpretBExpr (BUnary Not test) state
                afterElseStmt = interpretState elseStmt beforeElseStmt

        (While test bodyStmt) ->
            let
                bodySemantics = interpretState bodyStmt
                finalState = lfpFrom
                    state
                    (applyWidening
                        widening
                        (\x -> join
                            state
                            (bodySemantics $ interpretBExpr test x)
                        )
                    )
            in interpretBExpr (BUnary Not test) finalState
