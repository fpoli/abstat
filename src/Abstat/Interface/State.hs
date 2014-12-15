{-# LANGUAGE MultiParamTypeClasses #-}
module Abstat.Interface.State where

import Data.List

class State state domain where
    empty :: state domain
    store :: String -> domain -> state domain -> state domain
    load :: String -> state domain -> domain
    defined :: state domain -> [String]

fromList :: (State state domain) => [(String, domain)] -> state domain
fromList = foldr (uncurry store) empty

storeList :: (State state domain) => [(String, domain)] -> state domain -> state domain
storeList list state = foldr (uncurry store) state list

showVars :: (State state domain, Show domain) => [String] -> state domain -> String
showVars keys state =
    drop 1 $ intercalate "" $
        map
            (\k -> "\n    " ++ k ++ ": " ++ show (load k state))
            keys
