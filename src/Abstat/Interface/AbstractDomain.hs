{-# LANGUAGE MultiParamTypeClasses #-}
module Abstat.Interface.AbstractDomain where

import Abstat.Interface.Lattice

class (Lattice abstract) => AbstractDomain abstract where
    widening :: abstract -> abstract -> abstract
    widening _ b = b

    -- TODO: add narrowing?
