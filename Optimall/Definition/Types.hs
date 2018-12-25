module Optimall.Definition.Types
( Node
, NodeType (..)
, Hierarchy (..)
, Graph
, Template (..)
, TemplateCheck
, ShapeCheck
) where

import qualified Data.Map as Map

-- | Defines a Node, containing a value as well as some
-- information about the value.
data Node a = Node NodeType [Int] (Maybe a)

-- | Describes the type of a node; used mainly for the benefit
-- of the user, and for filtering graphs to find all nodes
-- of a particular type.
data NodeType = Input | Output | Parameter Int | Custom String

-- | A JSON-like structure where each level contains some
-- metadata.
data Hierarchy a b = Unit b a
                   | Keyed b (Map.Map String (Hierarchy a b))
                   | Ordered b [Hierarchy a b]

-- | Structures nodes and pairs them with a template
-- that describes how their values should be used.
type Graph a = Hierarchy (Node a) (Template a)

-- | Describes how the values in a graph interact with each other.
data Template a = Template { schema :: Hierarchy NodeType String
                           , templateCheck :: TemplateCheck a
                           , shapeCheck :: ShapeCheck a
                           , apply :: Graph a -> Graph a
                           , derivatives :: String -> Template a
                           }

-- | Returns any errors found with the types in a graph.
type TemplateCheck a = Hierarchy NodeType (Template a) -> [String]

-- | Returns any errors found with the shapes of tensors
-- in a graph.
type ShapeCheck a = Hierarchy [Int] (Template a) -> [String]
