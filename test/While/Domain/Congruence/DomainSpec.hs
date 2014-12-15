module While.Domain.Congruence.DomainSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.Hspec.QuickCheck
import Data.Maybe()
import Data.Ord()

import Abstat.Interface.PosetProp
import Abstat.Interface.LatticeProp
import Abstat.Interface.AbstractDomain()
import Abstat.Common.AbstractState
import Abstat.Common.Generator
import Abstat.Common.FlatDomainProp
import While.Domain.Congruence.Domain

spec :: Spec
spec =
    modifyMaxSuccess (*10) $
    modifyMaxDiscardRatio (*10) $
    modifyMaxSize (`div` 10) $
    parallel $ do

    describe "Domain" $ do
        testLatticeProperties (arbitrary :: Gen Domain)
        testPosetProperties (arbitrary :: Gen Domain)
        testFlatDomainProperties genInt

    describe "State Domain" $ do
        testLatticeProperties (arbitrary :: Gen (State Domain))
        testPosetProperties (arbitrary :: Gen (State Domain))
