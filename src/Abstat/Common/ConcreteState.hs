{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Abstat.Common.ConcreteState where

import qualified Data.Map.Strict as Map
import Test.QuickCheck
import Control.Monad (liftM, liftM2)

import qualified Abstat.Interface.State as Generic
import Abstat.Common.Generator

newtype State domain = State (Map.Map String domain)
    deriving (Eq,Show)

instance Generic.State State domain where
    empty = State Map.empty
    store key val (State s) = State $ Map.insert key val s
    load key (State s) = s Map.! key
    defined (State s) = Map.keys s

instance (Arbitrary domain) => Arbitrary (State domain) where
    arbitrary = liftM
        (State . Map.fromList)
        (listOf $ liftM2 (,) genVar arbitrary)

showState :: (Show domain) => State domain -> String
showState s@(State state) =
    Generic.showVars (Map.keys state) s
