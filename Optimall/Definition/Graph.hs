module Optimall.Definition.Graph
( Graph
, GraphPointer
, layout
, applyPointer
, copyGraph
, (>->)
) where

import qualified Data.Map as Map
import Optimall.Definition.Hierarchy
import Optimall.Definition.Pointer
import Optimall.Definition.Node
import Optimall.Definition.Template

-- | Redefine the Pointer type for simplicity.
type GraphPointer a = Pointer (Node a) (Template a)

-- | Create a data structure describing the
-- layout of the graph, including the template
-- type, names of keys, and the recommended type
-- of each node.
layout :: Graph a -> Schema
layout = hmap (nodeType) (name)

-- | Apply the subgraph specified by the given pointer.
applyPointer :: GraphPointer a -> Graph a -> Graph a
applyPointer (Pointer _ update) g = update f g
    where
        f g' = apply (metadata g') g'

-- | Copy the node values from one graph to another, but
-- keep the structure and node values the same.  Any nodes
-- present in the target but not in the source will be
-- left as they are; the original graph will be returned
-- in whole if the graph types are different.
copyGraph :: Graph a -> Graph a -> Graph a
copyGraph (Unit source _) (Unit target t) =
    Unit (copyNode source target) t
copyGraph (Keyed source _) (Keyed target t) =
    let f key target' = copyGraph (source Map.! key) target'
    in Keyed (Map.mapWithKey f target) t
copyGraph (Ordered source _) (Ordered target t) =
    let copyGraph' (source', target') = copyGraph source' target'
        n = length source
        (paired, unpaired)
            | length target <= n
                        = (zip source target, [])
            | otherwise = (zip source (take n target), drop n target)
        result = map (copyGraph') paired ++ unpaired
    in Ordered result t
copyGraph _ g = g

-- | Copy the node values from one graph to another, but
-- keep the structure and node values the same.  If all the
-- nodes in the target are not present in the source, an
-- error will be thrown.
(>->) :: Graph a -> Graph a -> Graph a
(>->) = copyGraph
