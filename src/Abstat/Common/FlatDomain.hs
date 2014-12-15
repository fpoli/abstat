module Abstat.Common.FlatDomain where

import Test.QuickCheck
import Control.Monad(liftM)

import Abstat.Interface.Lattice
import Abstat.Interface.AbstractDomain

data FlatDomain t
    = Top
    | Val t
    | Bottom
    deriving (Eq,Show,Ord)

instance (Eq t) => Lattice (FlatDomain t) where
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

instance (Eq t) => AbstractDomain (FlatDomain t) where
    -- nothing

instance (Arbitrary t) => Arbitrary (FlatDomain t) where
    arbitrary = frequency [
            (1, return Top),
            (3, liftM Val arbitrary),
            (1, return Bottom)
        ]
