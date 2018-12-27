module Optimall.Definition.Template
( Graph
, Template (..)
, Schema
) where

import Optimall.Definition.Hierarchy
import Optimall.Definition.Node

-- | Structures nodes and pairs them with a template
-- that describes how their values should be used.
type Graph a = Hierarchy (Node a) (Template a)

-- | Describes how the values in a graph interact with each other.
data Template a = Template { name :: String
                           , schema :: Schema
                           , check :: Graph a -> [String]
                           , apply :: Graph a -> Graph a
                           , derivatives :: String -> Template a
                           }

-- | Describes the layout of a graph or expected
-- layout of a template.
type Schema = Hierarchy NodeType String
