module Optimall.Definition.Pointer
( Pointer (..)
, resolve
, update
, (>:>)
, stackPointers
, keyPointer
, indexPointer
) where

import qualified Data.Map as Map
import Optimall.Definition.Hierarchy

-- | Contains functions for referencing and updating hierarchies.
data Pointer a b = Pointer (Hierarchy a b -> Maybe (Hierarchy a b))
    ((Hierarchy a b -> Hierarchy a b)
        -> Hierarchy a b -> Hierarchy a b)

-- | Extract the subhierarchy referenced by a pointer.  If
-- the subhierarchy does not exist, gives Nothing.
resolve :: Pointer a b -> Hierarchy a b -> Maybe (Hierarchy a b)
resolve (Pointer r _) = r

-- | Update the value stored in the location referenced by
-- a pointer according to some transfer function.  If the
-- subhierarchy does not exist, this will simply give back
-- the input hierarchy, unaltered.
update :: Pointer a b -> (Hierarchy a b -> Hierarchy a b)
    -> Hierarchy a b -> Hierarchy a b
update (Pointer _ u) = u

-- | Compose two pointers.
(>:>) :: Pointer a b -> Pointer a b -> Pointer a b
(>:>) (Pointer r u) (Pointer r' u') = Pointer r'' u''
    where
        r'' h = case r h of
            Just h' -> r' h'
            Nothing -> Nothing
        u'' f = u (u' f)

-- | Compose a list of pointers.
stackPointers :: [Pointer a b] -> Pointer a b
stackPointers = foldr1 (>:>)

-- | Create a pointer that accesses a certain key
-- of a keyed hierarchy.
keyPointer :: String -> Pointer a b
keyPointer k = Pointer r u
    where
        r (Keyed m _) = Map.lookup k m
        r _ = Nothing
        u f (Keyed m md) = Keyed (Map.adjust f k m) md
        u _ h = h

-- | Create a pointer that accesses a certain element
-- within an ordered hierarchy.  This is slow and may
-- cause errors, so is not recommended for general use.
indexPointer :: Int -> Pointer a b
indexPointer i = Pointer r u
    where
        r (Ordered l _)
            | length l > i = Just $ l !! i
            | otherwise    = Nothing
        r _ = Nothing
        u f (Ordered l md) =
            let l' = (take i l ++ [f $ l !! i] ++ drop (i + 1) l)
            in Ordered l' md
        u _ h = h
