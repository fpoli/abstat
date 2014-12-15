module While.Domain.Interval.DomainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.Maybe()
import Data.Ord()

import Abstat.Interface.PosetProp
import Abstat.Interface.LatticeProp
import Abstat.Interface.AbstractDomain()
import Abstat.Common.AbstractState
import While.Domain.Interval.Domain

spec :: Spec
spec =
    modifyMaxSuccess (*10) $
    modifyMaxDiscardRatio (*10) $
    modifyMaxSize (`div` 10) $
    parallel $ do

    describe "Domain" $ do
        testLatticeProperties (arbitrary :: Gen Domain)
        testPosetProperties (arbitrary :: Gen Domain)

    describe "State Domain" $ do
        testLatticeProperties (arbitrary :: Gen (State Domain))
        testPosetProperties (arbitrary :: Gen (State Domain))
