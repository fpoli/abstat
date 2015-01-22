module While.Domain.Sign.SemanticsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import System.Directory
import Data.List
import qualified Data.Map.Strict as Map

import Abstat.Interface.Lattice
import Abstat.Interface.Semantics
import Abstat.Interface.State hiding (State)
import Abstat.Common.AbstractState
import Abstat.Common.FlatDomain
import While.Parser
import While.AST
import While.AbstractSemantics
import While.Domain.Sign.Domain
import While.Domain.Sign.Semantics()
import While.SemanticsProp

spec :: Spec
spec =
    modifyMaxSuccess (*10) $
    modifyMaxDiscardRatio (*10) $
    modifyMaxSize (`div` 2) $
    parallel $ do

    describe "interpretAExpr" $ do
        modifyMaxSuccess (*10) $ testAExprSemanticsProperties
            (interpretAExpr :: AExpr -> State Domain -> Domain)

        it "interprets 3/5" $
            interpretAExpr (parseAExpression "3/5") (empty :: State Domain)
                `shouldBe` top

        it "interprets (-3)/5" $
            interpretAExpr (parseAExpression "(-3)/5") (empty :: State Domain)
                `shouldBe` top

    describe "interpretBExpr" $ do
        modifyMaxSuccess (*10) $ testBExprSemanticsProperties
            (interpretBExpr :: BExpr -> State Domain -> State Domain)

        it "interprets (false && ((5/0) < 10)" $
            interpretBExpr (parseBExpression "false && ((5/0) < 10)") (top :: State Domain)
                `shouldBe` bottom

        it "interprets (true || ((5/0) < 10)" $
            interpretBExpr (parseBExpression "true || ((5/0) < 10)") (top :: State Domain)
                `shouldBe` top

        it "interprets ((10<10) && ((5/0) < 10)" $
            interpretBExpr (parseBExpression "(10<10) && ((5/0) < 10)") (top :: State Domain)
                `shouldBe` bottom

    describe "interpretStmt" $ do
        modifyMaxSuccess (*10) $ testStmtSemanticsProperties
            (interpretState :: Stmt -> State Domain -> State Domain)

        context "programs from data/*.wl" $ do
            let expectated = Map.fromList [
                        ("count.wl", fromList [
                                ("value", Top)]),
                        ("ex1.wl", fromList [
                                ("x", Top)]),
                        ("ex2.wl", fromList [
                                ("x", Val Zero)]),
                        ("ex3.wl", bottom),
                        ("ex4.wl", fromList [
                                ("x", Top),
                                ("y", Top)]),
                        ("factorial.wl", fromList [
                                ("n", Val Positive),
                                ("tmp", Top),
                                ("factorial", Top)]),
                        ("fibonacci.wl", fromList [
                                ("fib", Val Positive),
                                ("i", Val Positive),
                                ("n", Val Positive),
                                ("pred1", Val Positive),
                                ("pred2", Val Positive)]),
                        ("loop.wl", bottom)
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
