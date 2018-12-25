module Optimall.Definition.Types
( Node
, NodeType (..)
, Graph
, Template (..)
, TemplateCheck
, ShapeCheck
) where

import Optimall.Definition.Hierarchy

-- | Defines a Node, containing a value as well as some
-- information about the value.
data Node a = Node NodeType [Int] (Maybe a)

-- | Describes the type of a node; used mainly for the benefit
-- of the user, and for filtering graphs to find all nodes
-- of a particular type.
data NodeType = Input | Output | Parameter Int | Custom String

-- | Structures nodes and pairs them with a template
-- that describes how their values should be used.
type Graph a = Hierarchy (Template a) (Node a)

-- | Describes how the values in a graph interact with each other.
data Template a = Template { schema :: Hierarchy String NodeType
                           , templateCheck :: TemplateCheck a
                           , shapeCheck :: ShapeCheck a
                           , apply :: Graph a -> Graph a
                           , derivatives :: String -> Template a
                           }

-- | Returns any errors found with the types in a graph.
type TemplateCheck a = Hierarchy (Template a) NodeType -> [String]

-- | Returns any errors found with the shapes of tensors
-- in a graph.
type ShapeCheck a = Hierarchy (Template a) [Int] -> [String]
