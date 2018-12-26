module Optimall.Definition.Hierarchy
( Hierarchy (..)
, hmap
, unitHierarchy
, keyedHierarchy
, orderedHierarchy
, (//)
, (/../)
, metadata
, adjustHierarchy
, adjustSubhierarchy
) where

import qualified Data.Map as Map

-- | A JSON-like structure where each level contains some
-- metadata.
data Hierarchy a b = Unit a b
                   | Keyed (Map.Map String (Hierarchy a b)) b
                   | Ordered [Hierarchy a b] b

-- | Map the data and metadata of an hierarchy.
hmap :: (a -> c) -> (b -> d) -> Hierarchy a b -> Hierarchy c d
hmap f fd (Unit u d) = Unit (f u) (fd d)
hmap f fd (Keyed m d) = Keyed (Map.map (hmap f fd) m) (fd d)
hmap f fd (Ordered l d) = Ordered (map (hmap f fd) l) (fd d)

-- | Index an element from the next level of the hierarchy.
(//) :: Hierarchy a b -> String -> Hierarchy a b
(//) (Keyed m _) k = m Map.! k
(//) (Ordered l _) k = l !! (read k)
(//) (Unit _ _) _ = error "Cannot index a unit hierarchy."

-- | Recursively index elements from the levels of
-- an hierarchy.
(/../) :: Hierarchy a b -> [String] -> Hierarchy a b
(/../) h [] = h
(/../) h (i:is) = (h // i) /../ is

-- | Adjust the hierarchy keyed by the given key
-- to a function of its old value.
adjustHierarchy :: (Hierarchy a b -> Hierarchy a b)
    -> String -> Hierarchy a b -> Hierarchy a b
adjustHierarchy f key (Keyed m md) =
    let newMap = Map.adjust (f) key m
    in Keyed newMap md
adjustHierarchy f index (Ordered l md) =
    let editList xs n f' = 
            let x = f' $ xs !! n
            in take n xs ++ [x] ++ drop (n + 1) xs
        newList = editList l (read index) f
    in Ordered newList md
adjustHierarchy _ _ (Unit _ _) =
    error "Cannot adjust elements of a unit hierarchy."

-- | Adjust the subhierarchy keyed by the given path
-- to a function of its old value.
adjustSubhierarchy :: (Hierarchy a b -> Hierarchy a b)
    -> [String] -> Hierarchy a b -> Hierarchy a b
adjustSubhierarchy f [] h = h
adjustSubhierarchy f (k:ks) h = 
    let f' = adjustSubhierarchy f ks
    in adjustHierarchy f' k h 

-- | Create a unit hierarcy from a tuple.
unitHierarchy :: (a, b) -> Hierarchy a b
unitHierarchy (unit, md) = Unit unit md

-- | Create a keyed hierarchy from a list of key-value pairs.
keyedHierarchy :: [(String, Hierarchy a b)] -> b -> Hierarchy a b
keyedHierarchy kvps md = Keyed (Map.fromList kvps) md

-- | Create an ordered hierarchy from a list of hierarchies.
orderedHierarchy :: [Hierarchy a b] -> b -> Hierarchy a b
orderedHierarchy ls md = Ordered ls md

-- | Extract the hierarchy's metadata at the top level.
metadata :: Hierarchy a b -> b
metadata (Unit _ md) = md
metadata (Keyed _ md) = md
metadata (Ordered _ md) = md
