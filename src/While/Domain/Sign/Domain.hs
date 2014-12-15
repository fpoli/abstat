{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module While.Domain.Sign.Domain where

import Test.QuickCheck hiding (Positive)

import Abstat.Interface.GaloisConnection
import Abstat.Common.FlatDomain

data Sign
    = Positive
    | Zero
    | Negative
    deriving (Eq,Show,Ord)

type Domain = FlatDomain Sign

instance GaloisConnection Integer Domain where
    concretization _ = error "not implemented"
    singleAbstraction n | n == 0 = Val Zero
    singleAbstraction n | n < 0 = Val Negative
    singleAbstraction _ = Val Positive

instance Arbitrary Sign where
    arbitrary = elements [Positive, Zero, Negative]
