{-# LANGUAGE MultiParamTypeClasses #-}
module Abstat.Interface.GaloisConnection where

import Abstat.Interface.Lattice

-- TODO: add Lattice ([] concrete) to context?

class (Lattice abstract) => GaloisConnection concrete abstract where
    concretization :: abstract -> [concrete]
    abstraction :: [concrete] -> abstract
    abstraction = foldr (\x res -> join (singleAbstraction x) res) bottom
    singleAbstraction :: concrete -> abstract
