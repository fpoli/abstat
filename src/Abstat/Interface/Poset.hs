{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Abstat.Interface.Poset where

import Abstat.Interface.Lattice

class Poset t where
    cmp :: t -> t -> Maybe Ordering
    cmp a b = case (a `leq` b, b `leq` a) of
        (True, True) -> Just EQ
        (True, False) -> Just LT
        (False, True) -> Just GT
        (False, False) -> Nothing

    lt :: t -> t -> Bool
    lt a b = cmp a b == Just LT

    leq :: t -> t -> Bool
    leq a b = cmp a b `elem` [Just EQ, Just LT]

    gt :: t -> t -> Bool
    gt a b = cmp a b == Just GT

    geq :: t -> t -> Bool
    geq a b = cmp a b `elem` [Just EQ, Just GT]

instance (Lattice t) => Poset t where
    cmp = cmpUsingMeet
