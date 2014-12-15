module While.ASTGenerator where

import Control.Monad
import Test.QuickCheck

import Abstat.Common.Generator
import While.AST

shrinkOne :: (Arbitrary a) => (a -> b) -> a -> [b]
shrinkOne ccc a = map ccc (shrink a)

shrinkTwo :: (Arbitrary a) => (a -> a -> b) -> a -> a -> [b]
shrinkTwo ccc a b = map (`ccc` b) (shrink a) ++ map (ccc a) (shrink b)

-- Generators
instance Arbitrary AExpr where
    arbitrary = frequency [
            (15, liftM Var genVar),
            (5, liftM IntConst genInt),
            (1, liftM2 AUnary arbitrary arbitrary),
            (10, liftM3 ABinary arbitrary arbitrary arbitrary)
        ]
    shrink val = case val of
        (IntConst _) -> []
        (Var _) -> atoms
        (AUnary op a) -> a : atoms ++ shrinkOne (AUnary op) a
        (ABinary op a b) -> [a, b] ++ atoms ++ shrinkTwo (ABinary op) a b
        where atoms = map IntConst [10, 3, 1, 0, -1, -3, -10]

instance Arbitrary AUnaOp where
    arbitrary = return Neg

instance Arbitrary ABinOp where
    arbitrary = elements [Add, Subtract, Multiply, Divide]

instance Arbitrary BExpr where
    arbitrary = frequency [
            (1, liftM BoolConst $ elements [True, False]),
            (5, liftM2 BUnary arbitrary arbitrary),
            (10, liftM3 BBinary arbitrary arbitrary arbitrary),
            (20, liftM3 RBinary arbitrary arbitrary arbitrary)
        ]
    shrink val = case val of
        (BoolConst _) -> []
        (BUnary op a) -> a : atoms ++ shrinkOne (BUnary op) a
        (BBinary op a b) -> [a, b] ++ atoms ++ shrinkTwo (BBinary op) a b
        (RBinary op a b) -> atoms ++ shrinkTwo (RBinary op) a b
        where atoms = [BoolConst True, BoolConst False]

instance Arbitrary BUnaOp where
    arbitrary = return Not

instance Arbitrary BBinOp where
    arbitrary = elements [And, Or]

instance Arbitrary RBinOp where
    arbitrary = elements [
            Equal,
            NotEqual,
            Greater,
            GreaterEqual,
            Less,
            LessEqual
        ]

instance Arbitrary Stmt where
    arbitrary = sized treeSize

treeSize :: Int -> Gen Stmt
treeSize 0 = return Skip
treeSize 1 = liftM2 Assign genVar arbitrary
treeSize x = frequency [
        (5, treeSeq x),
        (2, treeIf x),
        (1, treeWhile x)
    ]
    where
        treeSeq n = do
            rand <- choose (1, n-1)
            first <- treeSize rand
            second <- treeSize (n-1-rand)
            return $ Seq first second
        treeIf n = do
            rand <- choose (1, n-1)
            test <- arbitrary
            first <- treeSize rand
            second <- treeSize (n-1-rand)
            return $ If test first second
        treeWhile n = do
            test <- arbitrary
            first <- treeSize (n-1)
            return $ While test first
