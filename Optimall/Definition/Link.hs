module Optimall.Definition.Link
( Link (..)
, (-->)
, (/->)
, (-/>)
, (//>)
, resolveSource
, applyLink
, reverseLink
) where

import Optimall.Definition.Graph

-- | Defines a link between two subgraphs.
data Link = Link [String] [String]

-- | Specify a link between two top-level subgraphs.
(-->) :: String -> String -> Link
(-->) source target = Link [source] [target]

-- | Specify a link between a nested subgraph and
-- a top-level subgraph.
(/->) :: [String] -> String -> Link
(/->) source target = Link source [target]

-- | Specify a link between a top-level subgraph and a
-- nested subgraph.
(-/>) :: String -> [String] -> Link
(-/>) source target = Link [source] target

-- | Specify a link between two nested subgraphs.
(//>) :: [String] -> [String] -> Link
(//>) = Link

-- | Get the subgraph pointed to by the link's source.
resolveSource :: Graph a -> Link -> Graph a
resolveSource g (Link s _) = g /../ s

-- | Apply a link to a graph.
applyLink :: Graph a -> Link -> Graph a
applyLink g l@(Link source target) = 
    applyLink' (resolveSource g l) target g

-- | Given a source of a link and the target path of the link,
-- apply the link to the graph.
applyLink' :: Graph a -> [String] -> Graph a -> Graph a
applyLink' source path = 
    let replace _ = source
    in adjustSubgraph (replace) path

-- | Flip the direction of a link.
reverseLink :: Link -> Link
reverseLink (Link a b) = Link b a
