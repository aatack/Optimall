module Optimall.Definition.Template
( Graph
, Template (..)
, Schema
, TemplateCheck
, ShapeCheck
) where

import Optimall.Definition.Hierarchy
import Optimall.Definition.Node

-- | Structures nodes and pairs them with a template
-- that describes how their values should be used.
type Graph a = Hierarchy (Node a) (Template a)

-- | Describes how the values in a graph interact with each other.
data Template a = Template { name :: String
                           , schema :: Schema
                           , templateCheck :: TemplateCheck a
                           , shapeCheck :: ShapeCheck a
                           , apply :: Graph a -> Graph a
                           , derivatives :: String -> Template a
                           }

-- | Describes the layout of a graph or expected
-- layout of a template.
type Schema = Hierarchy NodeType String

-- | Returns any errors found with the types in a graph.
type TemplateCheck a = Hierarchy () (Template a) -> [String]

-- | Returns any errors found with the shapes of tensors
-- in a graph.
type ShapeCheck a = Hierarchy [Int] (Template a) -> [String]
