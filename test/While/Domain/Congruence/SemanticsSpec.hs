module While.Domain.Congruence.SemanticsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import System.Directory
import Data.List
import qualified Data.Map.Strict as Map

import Abstat.Interface.Lattice
import Abstat.Interface.Semantics
import Abstat.Interface.State hiding (State)
import Abstat.Common.AbstractState
import While.Parser
import While.AST
import While.AbstractSemantics
import While.Domain.Congruence.Domain
import While.Domain.Congruence.Semantics()
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

        it "interprets 1" $
            interpretAExpr (IntConst 1) (empty :: State Domain)
                `shouldBe` Val 1
        
        it "interprets x (undefined var)" $
            interpretAExpr (Var "x") (bottom :: State Domain)
                `shouldBe` bottom

        it "interprets 1 + 2" $
            interpretAExpr (parseAExpression "1 + 2") (bottom :: State Domain)
                `shouldBe` Val (3 `mod` moduleVal)
        
        it "interprets 1 + x (x undefined var)" $
            interpretAExpr (parseAExpression "1 + x") (bottom :: State Domain)
                `shouldBe` bottom

        it "interprets x / 0 (x undefined var)" $
            interpretAExpr (parseAExpression "x / 0") (top :: State Domain)
                `shouldBe` Top

        it "interprets x * 0 (x undefined var)" $
            interpretAExpr (parseAExpression "x * 0") (bottom :: State Domain)
                `shouldBe` bottom

        it "interprets (1+x) * (2*3-6) (x undefined var)" $
            interpretAExpr (parseAExpression "(1+x) * (2*3-6)") (bottom :: State Domain)
                `shouldBe` bottom

    describe "interpretBExpr" $ do
        modifyMaxSuccess (*10) $ testBExprSemanticsProperties
            (interpretBExpr :: BExpr -> State Domain -> State Domain)

        it "interprets (5/0) < 10" $
            interpretBExpr (parseBExpression "(5/0) < 10") (top :: State Domain)
                `shouldBe` top

        it "interprets (false && ((5/0) < 10)" $
            interpretBExpr (parseBExpression "false && ((5/0) < 10)") (top :: State Domain)
                `shouldBe` bottom

        it "interprets (false && (!((5/0) < 10))" $
            interpretBExpr (parseBExpression "false && ((5/0) < 10)") (top :: State Domain)
                `shouldBe` bottom

        it "interprets (true || ((5/0) < 10)" $
            interpretBExpr (parseBExpression "true || ((5/0) < 10)") (top :: State Domain)
                `shouldBe` top

        it "interprets (x == 2)" $
            interpretBExpr (parseBExpression "x == 2") (top :: State Domain)
                `shouldBe` storeList [("x", Val $ 2 `mod` moduleVal)] (top :: State Domain)

        it "interprets (not (x != 2))" $
            interpretBExpr (parseBExpression "not (x != 2)") (top :: State Domain)
                `shouldBe` storeList [("x", Val $ 2 `mod` moduleVal)] (top :: State Domain)

    describe "interpretState" $ do
        modifyMaxSuccess (*10) $ testStmtSemanticsProperties
            (interpretState :: Stmt -> State Domain -> State Domain)

        context "skip" $ do
            it "interprets on empty state" $
                interpretState Skip (empty :: State Domain)
                    `shouldBe` (empty :: State Domain)
            
            it "interprets on non-empty state" $
                interpretState Skip (store "x" (Val 10) (empty :: State Domain))
                    `shouldBe` store "x" (Val 10) (empty :: State Domain)
        
        context "assignment" $ do
            it "interprets x := 1 / 0" $
                interpretState (parseString "x := 1 / 0") (empty :: State Domain)
                    `shouldBe` store "x" top (empty :: State Domain)

            it "interprets x := 1" $
                interpretState (parseString "x := 1") (empty :: State Domain)
                    `shouldBe` store "x" (Val 1) (empty :: State Domain)

            it "interprets x := y" $
                interpretState (parseString "x := y") (empty :: State Domain)
                    `shouldBe` store "x" top (empty :: State Domain)

            it "interprets x := 1 + y" $
                interpretState (parseString "x := 1 + y") (empty :: State Domain)
                    `shouldBe` store "x" top (empty :: State Domain)

        context "seq" $
            it "interprets x := 1; x := 2" $
                interpretState (parseString "x := 1; x := 2") (empty :: State Domain)
                    `shouldBe` store "x" (Val $ 2 `mod` moduleVal) (empty :: State Domain)

        context "if" $ do
            it "interprets x := 2; if x == 2 then x := 3 else skip)" $
                interpretState
                    (parseString "x := 2; if x == 2 then x := 3 else skip")
                    (empty :: State Domain)
                    `shouldBe`
                    storeList [
                            ("x", top)
                        ] empty

            it "interprets if y == 1 then x := 2 else z := 3" $
                interpretState
                    (parseString "if y == 1 then x := 2 else z := 3")
                    (empty :: State Domain)
                    `shouldBe`
                    storeList [
                            ("x", top),
                            ("y", top),
                            ("z", top)
                        ] empty

            it "interprets y := 1; if y == 2 then x := 3/0 else x := 4" $
                interpretState
                    (parseString "y := 1; if y == 2 then x := 3/0 else x := 4")
                    (empty :: State Domain)
                    `shouldBe`
                    storeList [
                            ("x", Val $ 4 `mod` moduleVal),
                            ("y", Val 1)
                        ] empty

            it "interprets y := 1; if y == 1 then x := 2/0 else x := 3/0" $
                interpretState
                    (parseString "y := 1; if y == 1 then x := 2/0 else x := 3/0")
                    (empty :: State Domain)
                    `shouldBe`
                    storeList [
                            ("y", Val 1)
                        ] empty

        context "while" $ do
            it "interprets while x != 2 then x := 2" $
                interpretState
                    (parseString "while x != 2 do x := 2")
                    (empty :: State Domain)
                    `shouldBe`
                    store "x" (Val $ 2 `mod` moduleVal) (empty :: State Domain)

            it "interprets x := 1; while x == 1 do skip" $
                interpretState
                    (parseString "x := 1; while x == 1 do skip")
                    (empty :: State Domain)
                    `shouldBe`
                    store "x" (Val 1) (empty :: State Domain)

            it "interprets x := 0; while x == 0 do x := x + 1" $
                interpretState
                    (parseString "x := 0; while x == 0 do x := x + 1")
                    (empty :: State Domain)
                    `shouldBe`
                    store "x" Top (empty :: State Domain)

        it "interprets complex program" $
            interpretState
                (parseString "x := 0; while x == 0 do x := x + 1; if x < 10 then a := 2 else b := 3;")
                (empty :: State Domain)
                `shouldBe`
                storeList [
                        ("x", top),
                        ("a", top),
                        ("b", top)
                    ] (empty :: State Domain)

        context "programs from data/*.wl" $ do
            let expectated = Map.fromList [
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
