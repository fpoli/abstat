module While.Domain.Interval.SemanticsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import System.Directory
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe

import Abstat.Interface.Poset
import Abstat.Interface.Lattice
import Abstat.Interface.GaloisConnection
import Abstat.Interface.Semantics
import Abstat.Interface.State hiding (State)
import Abstat.Common.AbstractState
import While.AST
import While.Parser
import While.AbstractSemantics
import While.Domain.Interval.Domain
import While.Domain.Interval.Semantics()
import While.SemanticsProp
import qualified While.Concrete as Concrete

spec :: Spec
spec =
    modifyMaxSuccess (*10) $
    modifyMaxDiscardRatio (*10) $
    modifyMaxSize (`div` 2) $
    parallel $ do

    describe "interpretAExpr" $ do
        modifyMaxSuccess (*10) $ testAExprSemanticsProperties
            (interpretAExpr :: AExpr -> State Domain -> Domain)

        it "interprets c / ((-y)/x)" $ do
            let aexp = parseAExpression "c / ((-y)/x)"
            let vars = ["c","x","y"]
            let concreteVals1 = [9, 10, 14]
            let concreteVals2 = [6, 3, 3]
            let abstractVals = map
                    (\(x,y) -> abstraction [x,y])
                    (zip concreteVals1 concreteVals2)
                    :: [Domain]
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

            (abstraction (map fromJust [concreteResult1, concreteResult2])
                `leq` abstractResult) `shouldBe` True

    describe "interpretBExpr" $ do
        modifyMaxSuccess (*10) $ testBExprSemanticsProperties
            (interpretBExpr :: BExpr -> State Domain -> State Domain)

        it "interprets (x/d)*d == x" $
            interpretBExpr
                (parseBExpression "(x/d)*d == x")
                (fromList [
                    ("x", Interval (Val 100) (Val 100)),
                    ("d", Interval (Val 2) PlusInf)] :: State Domain)
            `shouldBe` fromList [
                ("x", Interval (Val 100) (Val 100)),
                ("d", Interval (Val 2) PlusInf)]

        it "interprets d <= x/d && s == 0" $
            interpretBExpr
                (parseBExpression "d <= x/d && s == 0")
                (fromList [
                    ("x", Interval (Val 100) PlusInf),
                    ("s", Interval (Val 0) (Val 1)),
                    ("d", Interval (Val 2) PlusInf)] :: State Domain)
            `shouldBe` fromList [
                ("x", Interval (Val 100) PlusInf),
                ("s", Interval (Val 0) (Val 1)),
                ("d", Interval (Val 2) PlusInf)]

        it "interprets s == 0 && x != 1" $
            interpretBExpr
                (parseBExpression "s == 0 && x != 1")
                (fromList [
                    ("x", Interval (Val 100) (Val 100)),
                    ("s", Interval (Val 0) (Val 1))] :: State Domain)
            `shouldBe` fromList [
                ("x", Interval (Val 100) (Val 100)),
                ("s", Interval (Val 0) (Val 1))]

        it "interprets not (s == 0 && x != 1)" $
            interpretBExpr
                (parseBExpression "not (s == 0 && x != 1)")
                (fromList [
                    ("x", Interval (Val 100) (Val 100)),
                    ("s", Interval (Val 0) (Val 1))] :: State Domain)
            `shouldBe` fromList [
                ("x", Interval (Val 100) (Val 100)),
                ("s", Interval (Val 0) (Val 1))]

        it "interprets s <> 0 || x == 1" $
            interpretBExpr
                (parseBExpression "s <> 0 || x == 1")
                (fromList [
                    ("x", Interval (Val 100) (Val 100)),
                    ("s", Interval (Val 0) (Val 1))] :: State Domain)
            `shouldBe` fromList [
                ("x", Interval (Val 100) (Val 100)),
                ("s", Interval (Val 0) (Val 1))]

        it "interprets s <> 0" $
            interpretBExpr
                (parseBExpression "s <> 0")
                (fromList [
                    ("s", Interval (Val 0) (Val 1))] :: State Domain)
            `shouldBe` fromList [
                ("s", Interval (Val 0) (Val 1))]

    describe "interpretStmt" $
        context "programs from data/*.wl" $ do
            let expectated = Map.fromList [
                        ("convert.wl", fromList [
                            ("input", Interval MinusInf (Val 600500553290)),
                            ("output", Interval MinusInf PlusInf),
                            ("position", Interval (Val 1) PlusInf),
                            ("remain", Interval MinusInf PlusInf),
                            ("shift", Interval (Val 10) (Val 10)),
                            ("tobase", Interval (Val 2) (Val 2))]),
                        ("findprime.wl", fromList [
                            ("current", Interval (Val 1000000000000) PlusInf),
                            ("divisor", top),
                            ("findafter", Interval (Val 1000000000000) (Val 1000000000000)),
                            ("firstprime", top),
                            ("found", Interval (Val 0) (Val 1)),
                            ("isdivisible", top)])
                    ] :: Map.Map String (State Domain)

            whileFiles <- runIO $ do
                allFiles <- getDirectoryContents "data"
                return $ filter (isSuffixOf ".wl") allFiles

            let testParseFile filename =
                    it ("interprets program data/" ++ filename) $ do
                        stmt <- parseFile $ "data/" ++ filename
                        let final = interpret stmt
                        case Map.lookup filename expectated of
                            Just result -> final `shouldBe` result
                            Nothing -> return ()

            mapM_ testParseFile whileFiles
