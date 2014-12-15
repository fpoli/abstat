{-# LANGUAGE MultiParamTypeClasses #-}
module Abstat.Interface.Semantics where

import Abstat.Interface.AST
import Abstat.Interface.State

class (AST ast, State state domain) =>
    Semantics ast state domain where

    interpretState :: ast -> state domain -> state domain
    interpret :: ast -> state domain
    interpret stmt = interpretState stmt empty
