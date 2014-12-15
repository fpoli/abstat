module Abstat.Common.BoolDomain where

import Prelude (Eq,Show,(==),Bool)
import qualified Prelude
import Abstat.Interface.Lattice
import Abstat.Interface.Poset
import Test.QuickCheck
import qualified Data.Set as Set

data BoolDomain
    = Top
    | True
    | False
    | Bottom
    deriving (Eq,Show)

instance Lattice BoolDomain where
    top = Top
    bottom = Bottom

    join a b | a == b = a
    join True False = Top
    join True Bottom = True
    join False True = Top
    join False Bottom = False
    join Bottom True = True
    join Bottom False = False
    join _ _ = Top

    meet a b | a == b = a
    meet True False = Bottom
    meet True Top = True
    meet False True = Bottom
    meet False Top = False
    meet Top True = True
    meet Top False = False
    meet _ _ = Bottom

instance Poset BoolDomain where
    cmp = cmpUsingMeet

instance Arbitrary BoolDomain where
    arbitrary = elements [Top, True, False, Bottom]

alpha :: Set.Set Bool -> BoolDomain
alpha set = Set.foldr (\x res -> join (beta x) res) bottom set

beta :: Bool -> BoolDomain
beta Prelude.True = True
beta Prelude.False = False
