module While.Domain.Int.DomainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.Maybe()
import Data.Ord()

import Abstat.Interface.Poset
import Abstat.Interface.PosetProp
import Abstat.Interface.Lattice
import Abstat.Interface.LatticeProp
import Abstat.Interface.AbstractDomain()
import Abstat.Interface.State hiding (State)
import Abstat.Common.AbstractState
import Abstat.Common.Generator
import Abstat.Common.FlatDomainProp
import While.Domain.Int.Domain

spec :: Spec
spec =
    modifyMaxSuccess (*10) $
    modifyMaxDiscardRatio (*10) $
    modifyMaxSize (`div` 10) $
    parallel $ do

    describe "Domain" $ do
        testLatticeProperties (arbitrary :: Gen Domain)
        testPosetProperties (arbitrary :: Gen Domain)
        testFlatDomainProperties genInt

        describe "lattice operations" $
            it "defines join (Val 1) bottom" $
                join (Val (1 :: Integer)) bottom `shouldBe` Val (1 :: Integer)

    describe "State Domain" $ do
        testLatticeProperties (arbitrary :: Gen (State Domain))
        testPosetProperties (arbitrary :: Gen (State Domain))

        let emptyState = empty :: State Domain
        describe "state operations" $ do
            it "defines empty" $
                defined emptyState `shouldBe` []

            it "defines load, store" $
                load "x" (store "x" Top emptyState)
                    `shouldBe` Top
            
            it "defines load, store when over-writing" $
                load "x" (store "x" bottom $ store "x" Top emptyState)
                    `shouldBe` bottom
            
            it "defines load on empty state" $
                load "x" (empty :: (State Domain)) `shouldBe` top
            
            it "defines store-bottom on empty state" $
                store "x" bottom emptyState
                    `shouldBe` (bottom :: State Domain)

            it "defines store-bottom on non-empty state" $
                store "x" bottom (store "x" (Val 3) emptyState)
                    `shouldBe` (bottom :: State Domain)

            it "defines defined" $
                defined (store "x" (Val 1) emptyState)
                    `shouldBe` ["x"]

        describe "lattice operations" $ do
            it "defines meet empty empty" $
                meet empty empty `shouldBe` emptyState

            it "defines join empty empty" $
                join empty empty `shouldBe` emptyState

            it "defines join non-empty non-empty" $
                join
                    (store "x" (Val 1) $ store "y" (Val 2) emptyState)
                    (store "x" (Val 3) $ store "z" (Val 4) emptyState)
                    `shouldBe`
                    store "x" Top (store "y" Top $ store "z" Top emptyState)

            it "defines meet non-empty non-empty" $
                meet
                    (store "x" (Val 1) $ store "y" (Val 2) emptyState)
                    (store "x" (Val 3) $ store "z" (Val 4) emptyState)
                    `shouldBe`
                    (bottom :: State Domain)

            it "defines join x := Val 1, x:= bottom)" $
                join (store "x" (Val 1) bottom) (store "x" bottom bottom)
                    `shouldBe` (store "x" (Val 1) bottom :: State Domain)

        describe "poset operations" $ do
            it "defines cmp when LT" $
                cmp emptyState (store "x" (Val 1) emptyState)
                    `shouldBe` Just GT

            it "defines cmp when GT" $
                cmp (store "x" (Val 2) emptyState) emptyState
                    `shouldBe` Just LT

            it "defines cmp when EQ - top" $
                cmp (store "x" top emptyState) emptyState
                    `shouldBe` Just EQ

            it "defines cmp when EQ - bottom" $
                cmp (store "x" bottom emptyState) bottom
                    `shouldBe` Just EQ

            it "defines cmp when NC" $
                cmp (store "x" (Val 3) emptyState) (store "x" (Val 5) emptyState)
                    `shouldBe` Nothing
