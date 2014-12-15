{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
module Abstat.Interface.AbstractSemantics where

import Abstat.Interface.Poset
import Abstat.Interface.Semantics
import Abstat.Interface.AbstractState
import Abstat.Interface.GaloisConnection

class (
        Semantics ast aState abstract,
        AbstractState aState abstract,
        GaloisConnection concrete abstract
    ) => AbstractSemantics ast concrete aState abstract where

instance (
        Semantics ast aState abstract,
        AbstractState aState abstract,
        GaloisConnection concrete abstract
    ) => AbstractSemantics ast concrete aState abstract where
    -- nothing

applyWidening :: (Poset t) => (t -> t -> t) -> (t -> t) -> t -> t
applyWidening widening fun x =
    case fun x of
        fx | fx `leq` x -> x
        fx -> widening x fx
