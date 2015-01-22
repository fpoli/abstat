{-# LANGUAGE FlexibleContexts #-}
module While.SemanticsProp where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Property
import qualified Data.Set as Set
import Control.Exception

import Abstat.Interface.Lattice
import Abstat.Interface.Poset
import Abstat.Interface.State
import Abstat.Interface.GaloisConnection
import Abstat.Interface.AST
import Abstat.Common.Generator
import qualified Abstat.Common.ConcreteState as Concrete
import While.AST
import While.ASTGenerator()
import qualified While.Concrete as Concrete

purifyException :: b -> IO (Maybe b)
purifyException v = protect (const Nothing) $ return . Just =<< evaluate v

testAExprSemanticsProperties ::
    (State aState d, Poset d, Show d, Eq d, Lattice d, Show (aState d),
        GaloisConnection Integer d) =>
    (AExpr -> aState d -> d) ->
    Spec

testAExprSemanticsProperties interpretAExpr =
    describe "Arithmetic semantics properties" $ do
        it "abstraction(concSem(x)) <= absSem(abstraction(x))" $ property $
            forAllShrink arbitrary shrink $ \aexp -> ioProperty $ do
                let vars = Set.toList $ usedVars aexp
                concreteVals <- mapM (\_ -> generate genInt) vars
                let abstractVals = map singleAbstraction concreteVals
                let concreteInitial = foldr
                        (\(key, value) state -> store key value state)
                        empty (zip vars concreteVals)
                let abstractInitial = foldr
                        (\(key, value) state -> store key value state)
                        empty (zip vars abstractVals)
                let abstractResult = interpretAExpr aexp abstractInitial
                concreteResult <- purifyException $
                    Concrete.interpretAExpr aexp concreteInitial
                
                return $ case concreteResult of
                    -- concrete interpretation failed with an exception
                    Nothing -> rejected
                    -- concrete interpretation gave a result
                    Just cr ->
                        liftBool $ singleAbstraction cr `leq` abstractResult

        it "abstraction({concSem(x), concSem(y)}) <= absSem(abstraction({x,y}))" $ property $
            forAllShrink arbitrary shrink $ \aexp ->
                let vars = Set.toList $ usedVars aexp
                in
                    forAll (vectorOf (length vars) genInt) $ \concreteVals1 ->
                    forAll (vectorOf (length vars) genInt) $ \concreteVals2 ->
                    ioProperty $ do
                        let abstractVals = map
                                (\(x,y) -> abstraction [x,y])
                                (zip concreteVals1 concreteVals2)
                        let concreteInitial1 = foldr
                                (\(key, value) state -> store key value state)
                                empty (zip vars concreteVals1)
                        let concreteInitial2 = foldr
                                (\(key, value) state -> store key value state)
                                empty (zip vars concreteVals2)
                        let abstractInitial = foldr
                                (\(key, value) state -> store key value state)
                                empty (zip vars abstractVals)
                        concreteResult1 <- purifyException $
                            Concrete.interpretAExpr aexp concreteInitial1
                        concreteResult2 <- purifyException $
                            Concrete.interpretAExpr aexp concreteInitial2
                        let abstractResult = interpretAExpr aexp abstractInitial
                        
                        return $ case (concreteResult1, concreteResult2) of
                            -- concrete interpretation failed with an exception
                            (Nothing, _) -> rejected
                            (_, Nothing) -> rejected
                            -- concrete interpretation gave a result
                            (Just cr1, Just cr2) ->
                                liftBool $ abstraction [cr1,cr2]
                                `leq` abstractResult

testBExprSemanticsProperties ::
    (State aState d, Poset d, Show d, Eq d, Lattice d, Show (aState d),
        GaloisConnection Integer d, Lattice (aState d),
        GaloisConnection (Concrete.State Integer) (aState d)
    ) =>
    (BExpr -> aState d -> aState d) ->
    Spec

testBExprSemanticsProperties interpretBExpr =
    describe "Boolean semantics properties" $
        it "interpretation is correct: abstraction({concSem(x)}) <= absSem(abstraction({x}))" $
        property $
            forAllShrink arbitrary shrink $ \bexp -> ioProperty $ do
                let vars = Set.toList $ usedVars bexp
                concreteVals <- mapM (\_ -> generate genInt) vars
                let abstractVals = map singleAbstraction concreteVals
                let concreteInitial = foldr
                        (\(key, value) state -> store key value state)
                        empty (zip vars concreteVals)
                let abstractInitial = foldr
                        (\(key, value) state -> store key value state)
                        empty (zip vars abstractVals)
                let abstractResult = interpretBExpr bexp abstractInitial
                concreteResult <- purifyException $
                    Concrete.interpretBExpr bexp concreteInitial

                return $ case concreteResult of
                    -- concrete interpretation failed with an exception
                    Nothing -> rejected
                    -- concrete interpretation gave a result
                    Just False -> rejected
                    Just True -> liftBool $
                        singleAbstraction concreteInitial `leq` abstractResult

testStmtSemanticsProperties ::
    (State aState d, Lattice (aState d), Show (aState d)) =>
    (Stmt -> aState d -> aState d) ->
    Spec

testStmtSemanticsProperties interpretState =
    describe "Semantics properties" $
        it "interpretation with bottom state is always bottom" $
        property $
            forAllShrink arbitrary shrink $ \stmt ->
                interpretState stmt bottom `shouldBe` bottom
