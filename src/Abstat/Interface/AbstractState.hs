{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts, FlexibleInstances, UndecidableInstances #-}
module Abstat.Interface.AbstractState where

import Abstat.Interface.AbstractDomain
import Abstat.Interface.State

class (
        AbstractDomain abstract,
        State abstractState abstract,
        AbstractDomain (AbstractState abstractState abstract)
    ) => AbstractState abstractState abstract where

instance (
        AbstractDomain abstract,
        State abstractState abstract,
        AbstractDomain (AbstractState abstractState abstract)
    ) => AbstractState abstractState abstract where
    -- nothing
