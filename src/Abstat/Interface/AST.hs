module Abstat.Interface.AST where

import qualified Data.Set as Set

class AST t where
    usedVars :: t -> Set.Set String
