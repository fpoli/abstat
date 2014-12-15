module While.ParserSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import System.Directory
import Data.List
import qualified Data.Map.Strict as Map

import While.AST
import While.ASTGenerator()
import While.Parser
import While.Compiler

spec :: Spec
spec =
    modifyMaxSuccess (*10) $
    modifyMaxDiscardRatio (*10) $
    modifyMaxSize (`div` 3) $
    parallel $ do

    describe "parseString" $ do
        
        it "parses an empty string" $
            parseString "" `shouldBe` Skip

        it "parses whitespaces-only string" $ do
            parseString " " `shouldBe` Skip
            parseString "\n" `shouldBe` Skip
            parseString "\r" `shouldBe` Skip
            parseString "\t" `shouldBe` Skip
            parseString "   " `shouldBe` Skip

        it "parses skip" $
            parseString "skip" `shouldBe` Skip

        it "parses skip with spaces and parentesis" $ do
            parseString " skip " `shouldBe` Skip
            parseString "(skip)" `shouldBe` Skip
            parseString "((skip))" `shouldBe` Skip
            parseString "(((skip)))" `shouldBe` Skip
            parseString " ( ( skip  )  )  " `shouldBe` Skip

        it "parses concatenated skips" $ do
            parseString "skip;skip" `shouldBe` Seq Skip Skip
            parseString "skip;skip;skip" `shouldBe` Seq Skip (Seq Skip Skip)
            parseString "skip;skip;skip;skip" `shouldBe` Seq Skip (Seq Skip (Seq Skip Skip))

        it "parses concatenated skips with spaces" $
            parseString "skip ; skip" `shouldBe` Seq Skip Skip

        it "parses program terminating with semicolon" $
            parseString "skip;" `shouldBe` Skip

        it "parses concatenated skips with simple parentesis" $ do
            parseString "skip;(skip);skip" `shouldBe` Seq Skip (Seq Skip Skip)
            parseString "skip;(skip;skip);skip" `shouldBe` Seq Skip (Seq (Seq Skip Skip) Skip)

        it "parses concatenated skips with complex parentesis" $ do
            parseString "skip;((skip));skip;skip" `shouldBe` Seq Skip (Seq Skip (Seq Skip Skip))
            parseString "skip;((skip;skip));skip" `shouldBe` Seq Skip (Seq (Seq Skip Skip) Skip)

        it "parses skip and while loop" $ do
            parseString "skip;while true do skip;" `shouldBe` Seq Skip (While (BoolConst True) Skip)
            parseString "skip; while true do skip;" `shouldBe` Seq Skip (While (BoolConst True) Skip)

        it "parses while loop and skip" $
            parseString "while true do skip; skip" `shouldBe` Seq (While (BoolConst True) Skip) Skip

        it "parses assignment and while loop" $
            parseString "x := 5; while true do skip;" `shouldBe`
                Seq
                    (Assign "x" (IntConst 5))
                    (While (BoolConst True) Skip)

        it "parses assignment and complex while loop" $
            parseString "value := -5; while value < 5 do( value := 1 + value; );" `shouldBe`
                Seq
                    (Assign "value" (AUnary Neg (IntConst 5)))
                    (While
                        (RBinary Less (Var "value") (IntConst 5))
                        (Assign "value" (ABinary Add (IntConst 1) (Var "value")))
                    )

        it "parses simple condition" $ do
            parseString "if false then skip else skip" `shouldBe`
                If (BoolConst False) Skip Skip
            parseString "if false then skip; else skip;" `shouldBe`
                If (BoolConst False) Skip Skip

        it "parses simple condition and skip" $ do
            parseString "if false then skip else skip; skip" `shouldBe`
                Seq (If (BoolConst False) Skip Skip) Skip
            parseString "if false then skip; else skip; skip" `shouldBe`
                Seq (If (BoolConst False) Skip Skip) Skip

        it "parses condition with complex test" $
            parseString "if (x-5) < (x/2) then skip else skip" `shouldBe`
                If
                    (RBinary Less
                        (ABinary Subtract (Var "x") (IntConst 5))
                        (ABinary Divide (Var "x") (IntConst 2)))
                    Skip
                    Skip

        context "when used to parse the rapresentation of an AST" $ do
            it "produces the same AST" $
                let tree = Seq (Seq Skip Skip) (Seq Skip Skip)
                in (parseString . compileStmt) tree `shouldBe` tree

            it "produces the same AST randomly generated" $ property $
                \tree -> (parseString . compileStmt) tree == tree 

    describe "parseFile" $
        context "files in data/*.wl" $ do
            let expectated = Map.fromList [
                        ("loop.wl", While (BoolConst True) Skip)
                    ]

            whileFiles <- runIO $ do
                allFiles <- getDirectoryContents "data"
                return $ filter (isSuffixOf ".wl") allFiles

            let testParseFile filename =
                    it ("parses without errors file data/"++filename) $ do
                        stmt <- parseFile ("data/"++filename)
                        case Map.lookup filename expectated of
                            Just result -> stmt `shouldBe` result
                            Nothing -> return ()

            mapM_ testParseFile whileFiles
