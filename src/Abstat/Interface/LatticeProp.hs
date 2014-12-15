module Abstat.Interface.LatticeProp where

import Test.Hspec
import Test.QuickCheck

import Abstat.Interface.Lattice

testLatticeProperties :: (Lattice p, Eq p, Show p) => Gen p -> Spec
testLatticeProperties gen =
    describe "Lattice properties" $ do
        it "commutative law - join" $ property $
            forAll gen $ \a -> forAll gen $ \b ->
            a `join` b == b `join` a

        it "commutative law - meet" $ property $
            forAll gen $ \a -> forAll gen $ \b ->
            a `meet` b == b `meet` a

        it "associative law - join" $ property $
            forAll gen $ \a -> forAll gen $ \b -> forAll gen $ \c ->
            a `join` (b `join` c) == (a `join` b) `join` c

        it "associative law - meet" $ property $
            forAll gen $ \a -> forAll gen $ \b -> forAll gen $ \c ->
            a `meet` (b `meet` c) == (a `meet` b) `meet` c

        it "absorption law - 1" $ property $
            forAll gen $ \a -> forAll gen $ \b ->
            a `join` (a `meet` b) == a

        it "absorption law - 2" $ property $
            forAll gen $ \a -> forAll gen $ \b ->
            a `meet` (a `join` b) == a

        it "idempotent law - join" $ property $
            -- derives from absorption laws
            forAll gen $ \a -> a `join` a == a

        it "idempotent law - meet" $ property $
            -- derives from absorption laws
            forAll gen $ \a -> a `meet` a == a

        it "top properties" $ property $
            forAll gen $ \a ->
            top `join` a == top

        it "bottom properties" $ property $
            forAll gen $ \a ->
            bottom `meet` a == bottom
