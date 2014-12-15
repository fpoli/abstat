module Abstat.Common.Generator where

import Control.Monad
import Test.QuickCheck

genVar :: Gen String
genVar = frequency [
        (20, elements ["x", "y", "z"]),
        (10, elements ["a", "b", "c"]),
        (1,  liftM
            (\var -> "xxx"++var)
            (listOf1 $ elements ['a'..'z']))
    ]

genInt :: Gen Integer
genInt = frequency [
        (30, choose (0, 5)),
        (20, choose (6, 20)),
        (5,  choose (21, 100)),
        (1,  choose (101, 4294967296))
    ]
