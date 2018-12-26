module Optimall.Definition.Graph
( Graph
, layout
, templateLayout
, shapeLayout
, copyGraph
, (>->)
, (//)
, (/../)
, adjustGraph
, adjustSubgraph
) where

import qualified Data.Map as Map
import Optimall.Definition.Hierarchy
import Optimall.Definition.Node
import Optimall.Definition.Template

-- | Create a data structure describing the
-- layout of the graph, including the template
-- type, names of keys, and the recommended type
-- of each node.
layout :: Graph a -> Schema
layout = hmap (nodeType) (name)

-- | Create a data structure describing the templates which
-- make up the graph.
templateLayout :: Graph a -> Hierarchy () (Template a)
templateLayout =
    let nullify _ = ()
    in hmap (nullify) (id)

-- | Create a data structure describing the shapes and
-- templates which make up the graph.
shapeLayout :: Graph a -> Hierarchy [Int] (Template a)
shapeLayout = hmap (shape) (id)

-- | Apply the values contained within a graph according to
-- its template.
applyGraph :: Graph a -> Graph a
applyGraph g = apply (metadata g) g

-- | Update the graph by applying the template to the
-- subgraph specified by the path.
applySubgraph :: [String] -> Graph a -> Graph a
applySubgraph [] g = applyGraph g
applySubgraph (p:ps) g = applySubgraph ps (g // p)

-- | Copy the node values from one graph to another, but
-- keep the structure and node values the same.  If all the
-- nodes in the target are not present in the source, an
-- error will be thrown.
copyGraph :: Graph a -> Graph a -> Graph a
copyGraph (Unit source _) (Unit target t) =
    Unit (copyNode source target) t
copyGraph (Keyed source _) (Keyed target t) =
    let f key target' = copyGraph (source Map.! key) target'
    in Keyed (Map.mapWithKey f target) t
copyGraph (Ordered source _) (Ordered target t) =
    let copyGraph' (source', target') = copyGraph source' target'
    in Ordered (map (copyGraph') (zip source target)) t
copyGraph _ _ = error "Cannot copy between graphs of different types."

-- | Copy the node values from one graph to another, but
-- keep the structure and node values the same.  If all the
-- nodes in the target are not present in the source, an
-- error will be thrown.
(>->) :: Graph a -> Graph a -> Graph a
(>->) = copyGraph

-- | Adjust the graph keyed by the given key
-- to a function of its old value.
adjustGraph :: (Graph a -> Graph a) -> String
    -> Graph a -> Graph a
adjustGraph = adjustHierarchy

-- | Adjust the graph keyed by the given path
-- to a function of its old value.
adjustSubgraph :: (Graph a -> Graph a) -> [String]
    -> Graph a -> Graph a
adjustSubgraph = adjustSubhierarchy
