{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}
module Abstat.Common.FlatDomainProp where

import Test.Hspec
import Test.QuickCheck

import Abstat.Interface.Poset
import Abstat.Common.FlatDomain

testFlatDomainProperties ::
    forall domain .
    (Eq domain, Show domain) =>
    Gen domain -> Spec

testFlatDomainProperties gen =
    describe "FlatDomain properties" $
        describe "Poset properties" $ do
            it "defines cmp when EQ" $
                cmp (Bottom :: FlatDomain domain) Bottom `shouldBe` Just EQ 
            it "defines cmp when EQ" $
                cmp (Top :: FlatDomain domain) Top `shouldBe` Just EQ
            it "defines cmp when EQ" $ property $
                forAll gen $ \a -> forAll gen $ \b ->
                a == b ==> cmp (Val a) (Val a) `shouldBe` Just EQ

            it "defines cmp when LT" $
                cmp (Bottom :: FlatDomain domain) Top `shouldBe` Just LT
            it "defines cmp when LT" $ property $
                forAll gen $ \a ->
                cmp (Val a) Top `shouldBe` Just LT
            it "defines cmp when LT" $ property $
                forAll gen $ \a ->
                cmp Bottom (Val a) `shouldBe` Just LT
            
            it "defines cmp when GT" $
                cmp (Top :: FlatDomain domain) Bottom `shouldBe` Just GT
            it "defines cmp when GT" $
                forAll gen $ \a ->
                cmp Top (Val a) `shouldBe` Just GT
            it "defines cmp when GT" $ property $
                forAll gen $ \a ->
                cmp (Val a) Bottom `shouldBe` Just GT
            
            it "defines cmp when NC" $ property $
                forAll gen $ \a -> forAll gen $ \b ->
                a /= b ==> cmp (Val a) (Val b) `shouldBe` Nothing
