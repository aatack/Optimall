module Optimall.Definition.Node
( Node (..)
, NodeType (..)
, (<<)
) where

-- | Defines a Node, containing a value as well as some
-- information about the value.
data Node a = Node { nodeType :: NodeType
                   , shape :: [Int]
                   , value :: (Maybe a)
                   }

-- | Describes the type of a node; used mainly for the benefit
-- of the user, and for filtering graphs to find all nodes
-- of a particular type.
data NodeType = Input | Output | Parameter Int | Custom String

-- | Set the value of a node.
(<<) :: Node a -> a -> Node a
(<<) (Node t s _) x = Node t s (Just x)
