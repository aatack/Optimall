module Optimall.Definition.Graph
( Graph
, layout
, templateLayout
, shapeLayout
) where

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
