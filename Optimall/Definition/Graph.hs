module Optimall.Definition.Graph
( Graph
, layout
, templateLayout
, shapeLayout
) where

import Optimall.Definition.Hierarchy
import Optimall.Definition.Node
import Optimall.Definition.Internal

-- | Create a data structure describing the
-- layout of the graph, including the template
-- type, names of keys, and the recommended type
-- of each node.
layout :: Graph a -> Schema
layout = 
    let nullify _ = ()
    in hmap (nodeType) (nullify)

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
