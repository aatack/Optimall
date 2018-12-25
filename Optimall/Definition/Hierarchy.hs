module Optimall.Definition.Hierarchy
( Hierarchy (..)
, unitHierarchy
, keyedHierarchy
, orderedHierarchy
, (//)
, (//!)
) where

import qualified Data.Map as Map

-- | A JSON-like structure where each level contains some
-- metadata.
data Hierarchy a b = Unit a b
                   | Keyed (Map.Map String (Hierarchy a b)) b
                   | Ordered [Hierarchy a b] b

-- | Index an element from the next level of the hierarchy.
(//) :: Hierarchy a b -> String -> Hierarchy a b
(//) (Keyed m _) k = m Map.! k
(//) (Ordered l _) k = l !! (read k :: Int)
(//) (Unit _ _) _ = error "Cannot index a unit hierarchy."

-- | Recursively index elements from the levels of
-- a hierarchy.
(//!) :: Hierarchy a b -> [String] -> Hierarchy a b
(//!) h [] = h
(//!) h (i:is) = (h // i) //! is

-- | Create a unit hierarcy from a tuple.
unitHierarchy :: (a, b) -> Hierarchy a b
unitHierarchy (unit, metadata) = Unit unit metadata

-- | Create a keyed hierarchy from a list of key-value pairs.
keyedHierarchy :: [(String, Hierarchy a b)] -> b -> Hierarchy a b
keyedHierarchy kvps metadata = Keyed (Map.fromList kvps) metadata

-- | Create an ordered hierarchy from a list of hierarchies.
orderedHierarchy :: [Hierarchy a b] -> b -> Hierarchy a b
orderedHierarchy ls metadata = Ordered ls metadata
