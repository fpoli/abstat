{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
module Abstat.Common.AbstractState where

import qualified Data.Map.Strict as Map
import Test.QuickCheck
import Control.Monad(liftM,liftM2)

import Abstat.Interface.Lattice
import Abstat.Interface.State hiding (State)
import qualified Abstat.Interface.State as Generic
import Abstat.Interface.AbstractDomain
import Abstat.Interface.GaloisConnection
import Abstat.Common.Generator

data State abstract
    = State (Map.Map String abstract)
    | Bottom
    deriving (Show,Eq)

normalizeState :: (Lattice abstract, Ord abstract) => State abstract -> State abstract
normalizeState Bottom = Bottom
normalizeState (State s) =
    stateWithoutTopBottom
    where
        mapWithoutTop =
            Map.fromList $ filter (\(_, v) -> v /= top) $ Map.toList s
        stateWithoutTopBottom =
            Map.foldr (\v res -> if v == bottom then Bottom else res) (State mapWithoutTop) mapWithoutTop

instance (Ord abstract, Lattice abstract) => Generic.State State abstract where
    empty = State Map.empty
    store _ _ Bottom = Bottom
    store key val (State s) = normalizeState $ State $ Map.insert key val s
    load _ Bottom = bottom
    load key (State s) = Map.findWithDefault top key s
    defined Bottom = []
    defined (State s) = Map.keys s

instance (Ord abstract, Lattice abstract) => Lattice (State abstract) where
    join x Bottom = x
    join Bottom y = y
    join (State a) (State b) = normalizeState $
        State $ Map.intersectionWith join a b
    meet _ Bottom = Bottom
    meet Bottom _ = Bottom
    meet (State a) (State b) = normalizeState $
        State $ Map.unionWith meet a b
    top = State Map.empty
    bottom = Bottom

instance (
        Show abstract,
        Ord abstract,
        Lattice (State abstract),
        AbstractDomain abstract
    ) => AbstractDomain (State abstract) where

    widening Bottom (State b) = error $ "widening Bottom (State " ++ show b ++ ")"
    widening _ Bottom = Bottom
    widening (State a) (State b) = normalizeState $
        State $ Map.mergeWithKey
                (\_ x y -> Just $ widening x y)
                (Map.map (`widening` top))
                (Map.map (top `widening`))
                a b

instance (
        Ord abstract,
        GaloisConnection concrete abstract,
        Generic.State cState concrete,
        AbstractDomain abstract
    ) => GaloisConnection (cState concrete) (State abstract) where

    concretization = error "not implemented"
    singleAbstraction concreteState = normalizeState $
        State $ Map.fromList $ map
            (\key -> (key, singleAbstraction $ load key concreteState))
            (defined concreteState)

removeValues :: (Eq v, Ord k) => v -> Map.Map k v -> Map.Map k v
removeValues targetVal dict =
    foldr (Map.update updateValue) dict (Map.keys dict)
    where updateValue val =
            if val == targetVal
            then Nothing
            else Just val

instance (
        Ord abstract,
        Lattice abstract,
        Arbitrary abstract
    ) => Arbitrary (State abstract) where

    arbitrary = liftM
        (normalizeState . State . Map.fromList)
        (listOf $ liftM2 (,) genVar arbitrary)
