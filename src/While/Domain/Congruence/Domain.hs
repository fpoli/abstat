{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module While.Domain.Congruence.Domain where

import Test.QuickCheck
import Control.Monad(liftM)

import Abstat.Interface.Lattice
import Abstat.Interface.AbstractDomain
import Abstat.Interface.GaloisConnection

moduleVal :: Integer
moduleVal = 2

data Domain
    = Top
    | Val Integer
    | Bottom
    deriving (Eq,Show,Ord)

instance Lattice Domain where
    top = Top
    bottom = Bottom

    join a b | a == b = a
    join (Val n) Bottom = Val n
    join Bottom (Val n) = Val n
    join _ _ = Top

    meet a b | a == b = a
    meet (Val n) Top = Val n
    meet Top (Val n) = Val n
    meet _ _ = Bottom

instance AbstractDomain Domain where
    -- nothing

instance Arbitrary Domain where
    arbitrary = frequency [
            (1, return Top),
            (3, liftM Val arbitrary),
            (1, return Bottom)
        ]

instance GaloisConnection Integer Domain where
    concretization _ = error "not implemented"
    singleAbstraction x = Val $ x `mod` moduleVal
