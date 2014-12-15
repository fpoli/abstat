module Abstat.Interface.PosetProp where

import Test.Hspec
import Test.QuickCheck

import Abstat.Interface.Poset

testPosetProperties :: (Poset p, Eq p, Show p) => Gen p -> Spec
testPosetProperties gen =
    describe "Poset properties" $ do
        it "reflexivity" $
            forAll gen $ \a -> a `leq` a

        it "antisymmetry" $
            forAll gen $ \a -> forAll gen $ \b ->
            (a `leq` b && b `leq` a) ==> a == b

        it "transitivity" $
            forAll gen $ \a -> forAll gen $ \b -> forAll gen $ \c ->
            (a `leq` b && b `leq` c) ==> a `leq` c
