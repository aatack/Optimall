module Optimall.Definition.Pointer
( Pointer (..)
, resolve
, update
, (>:>)
, (>::>)
) where

import qualified Data.Map as Map
import Optimall.Definition.Hierarchy

-- | Contains functions for referencing and updating hierarchies.
data Pointer a b = Pointer (Hierarchy a b -> Hierarchy a b)
    ((Hierarchy a b -> Hierarchy a b)
        -> Hierarchy a b -> Hierarchy a b)

-- | Extract the subhierarchy referenced by a pointer.
resolve :: Pointer a b -> Hierarchy a b -> Hierarchy a b
resolve (Pointer r _) = r

-- | Update the value stored in the location referenced by
-- a pointer according to some transfer function.
update :: Pointer a b -> (Hierarchy a b -> Hierarchy a b)
    -> Hierarchy a b -> Hierarchy a b
update (Pointer _ u) = u

-- | Compose two pointers.
(>:>) :: Pointer a b -> Pointer a b -> Pointer a b
(>:>) (Pointer r u) (Pointer r' u') = Pointer r'' u''
    where
        r'' = r' . r
        u'' f = u (u' f)

-- | Compose a list of pointers.
(>::>) :: [Pointer a b] -> Pointer a b
(>::>) = foldr1 (>:>)

-- | Create a pointer that accesses a certain key
-- of a keyed hierarchy.
keyPointer :: String -> Pointer a b
keyPointer k = Pointer r u
    where
        r (Keyed m _) = m Map.! k
        r _ = error ""
        u f (Keyed m md) = Keyed (Map.adjust f k m) md
        u _ _ = error ""

-- | Create a pointer that accesses a certain element
-- within an ordered hierarchy.  This is slow and may
-- cause errors, so is not recommended for general use.
indexPointer :: Int -> Pointer a b
indexPointer i = Pointer r u
    where
        r (Ordered l _) = l !! i
        r _ = error ""
        u f (Ordered l md) =
            let l' = (take i l ++ [f $ l !! i] ++ drop (i + 1) l)
            in Ordered l' md
        u _ _ = error ""
