module Optimall.Definition.Node
( Node (..)
, NodeType (..)
, (<-<)
, (<?<)
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
(<-<) :: Node a -> a -> Node a
(<-<) (Node t s _) x = Node t s (Just x)

-- | Set the value of a node, allowing for the fact that
-- it might have no value.
(<?<) :: Node a -> Maybe a -> Node a
(<?<) (Node t s _) m = Node t s m

-- | Copy the value of one node to another node, without
-- checking that the shapes match.  If the source node has
-- no value, any value the target node has will be overwritten.
(<<) :: Node a -> Node a -> Node a
(<<) (Node _ _ source) target = target <?< source
