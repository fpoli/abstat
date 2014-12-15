{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module While.Domain.Interval.Domain where

import Test.QuickCheck
import Control.Monad(liftM,liftM2)

import Abstat.Interface.Lattice
import Abstat.Interface.AbstractDomain
import Abstat.Interface.GaloisConnection
import Abstat.Common.Generator

data Domain
    = Interval Value Value
    | Bottom
    deriving (Eq,Show,Ord)

data Value
    = Val Integer
    | PlusInf
    | MinusInf
    deriving (Eq,Show)

instance Num Value where
    (+) PlusInf PlusInf = PlusInf
    (+) PlusInf MinusInf = error "PlusInf + MinusInf"
    (+) MinusInf PlusInf = error "MinusInf + PlusInf"
    (+) MinusInf MinusInf = MinusInf
    (+) _ PlusInf = PlusInf
    (+) PlusInf _ = PlusInf
    (+) _ MinusInf = MinusInf
    (+) MinusInf _ = MinusInf
    (+) (Val a) (Val b) = Val $ a + b
    
    (*) PlusInf PlusInf = PlusInf
    (*) PlusInf MinusInf = MinusInf
    (*) MinusInf PlusInf = MinusInf
    (*) MinusInf MinusInf = PlusInf
    (*) (Val a) PlusInf =
        case a `compare` 0 of
            GT -> PlusInf
            EQ -> Val 0
            LT -> MinusInf
    (*) PlusInf (Val b) =
        case b `compare` 0 of
            GT -> PlusInf
            EQ -> Val 0
            LT -> MinusInf
    (*) (Val a) MinusInf =
        case a `compare` 0 of
            GT -> MinusInf
            EQ -> Val 0
            LT -> PlusInf
    (*) MinusInf (Val b) =
        case b `compare` 0 of
            GT -> MinusInf
            EQ -> Val 0
            LT -> PlusInf
    (*) (Val a) (Val b) = Val $ a * b
    
    abs MinusInf = PlusInf
    abs PlusInf = MinusInf
    abs (Val a) = Val $ abs a
    
    signum MinusInf = Val (-1)
    signum PlusInf = Val 1
    signum (Val a) = Val $ signum a
    
    fromInteger = Val

    negate MinusInf = PlusInf
    negate PlusInf = MinusInf
    negate (Val a) = Val $ negate a

splitAtZero :: Domain -> (Domain, Domain)
splitAtZero Bottom = (Bottom, Bottom)
splitAtZero (Interval a b) =
    case (a <= 0, b >= 0) of
        (False, _) -> (Bottom, Interval a b)
        (True, False) -> (Interval a b, Bottom)
        (True, True) -> 
            case (a == 0, b == 0) of
                (True, True) -> (Bottom, Bottom)
                (True, False) -> (Bottom, Interval (Val 1) b)
                (False, True) -> (Bottom, Interval a (Val (-1)))
                (False, False) -> (Interval a (Val (-1)), Interval (Val 1) b)

instance Ord Value where
    (<=) a b | a == b = True
    (<=) MinusInf _ = True
    (<=) _ MinusInf = False
    (<=) PlusInf _ = False
    (<=) _ PlusInf = True
    (<=) (Val a) (Val b) = a <= b

instance Lattice Domain where
    top = Interval MinusInf PlusInf
    bottom = Bottom

    join Bottom Bottom = Bottom
    join a@(Interval _ _) b@(Interval _ _) | a == b = a
    join a@(Interval _ _) Bottom = a
    join Bottom b@(Interval _ _) = b
    join (Interval a b) (Interval c d) = Interval (min a c) (max b d)

    meet _ Bottom = Bottom
    meet Bottom _ = Bottom
    meet (Interval a b) (Interval c d) =
        if left > right
        then Bottom
        else Interval left right
        where
            left = max a c
            right = min b d

instance AbstractDomain Domain where
    widening (Interval a b) precise@(Interval c d) =
        if height <= 10
        then precise
        else
            Interval
                (if c < a
                then MinusInf
                else c)
                (if d > b
                then PlusInf
                else d)
        where
            height = d-c+1
    widening _ b = b

instance GaloisConnection Integer Domain where
    concretization _ = error "not implemented"
    singleAbstraction n = Interval (Val n) (Val n)

instance Arbitrary Value where
    arbitrary = frequency [
            (4, liftM Val genInt),
            (1, return MinusInf),
            (1, return PlusInf)
        ]

instance Arbitrary Domain where
    arbitrary = frequency [
            (3, suchThat
                (liftM2 Interval arbitrary arbitrary)
                (\(Interval a b) -> a <= b)),
            (1, return Bottom)
        ]
