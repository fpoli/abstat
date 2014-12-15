module Abstat.Interface.Lattice where

class (Eq t) => Lattice t where
    join :: t -> t -> t
    meet :: t -> t -> t
    top :: t
    bottom :: t

cmpUsingJoin :: (Eq t, Lattice t) => t -> t -> Maybe Ordering
cmpUsingJoin a b
    | a == b = Just EQ
    | otherwise =
        case a `join` b of
            x | x == a -> Just GT
            x | x == b -> Just LT
            _ -> Nothing

cmpUsingMeet :: (Eq t, Lattice t) => t -> t -> Maybe Ordering
cmpUsingMeet a b
    | a == b = Just EQ
    | otherwise =
        case a `meet` b of
            x | x == a -> Just LT
            x | x == b -> Just GT
            _ -> Nothing

lfpFrom :: (Eq t) => t -> (t -> t) -> t
lfpFrom initial fun =
    case fun initial of
        x | x == initial -> x
        x -> lfpFrom x fun
