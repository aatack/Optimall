module Optimall.Definition.Link
( Link (..)
, (-->)
, (/->)
, (-/>)
, (//>)
, resolveLink
, applyLink
, reverseLink
) where

import Optimall.Definition.Pointer
import Optimall.Definition.Graph

-- | Defines a link between two subgraphs.
data Link a = Link (GraphPointer a) (GraphPointer a)

-- | Specify a link between two top-level subgraphs.
(-->) :: String -> String -> Link a
(-->) source target = Link (keyPointer source) (keyPointer target)

-- | Specify a link between a nested subgraph and
-- a top-level subgraph.
(/->) :: [String] -> String -> Link a
(/->) source target = Link (keyPointers source) (keyPointer target)

-- | Specify a link between a top-level subgraph and a
-- nested subgraph.
(-/>) :: String -> [String] -> Link a
(-/>) source target = Link (keyPointer source) (keyPointers target)

-- | Specify a link between two nested subgraphs.
(//>) :: [String] -> [String] -> Link a
(//>) source target = Link (keyPointers source) (keyPointers target)

-- | Create a pointer by composing key pointers
-- for each part of the path.
keyPointers :: [String] -> GraphPointer a
keyPointers path = stackPointers . map (keyPointer) $ path

-- | Index the link's source and target for a
-- specific graph.
resolveLink :: Link a -> Graph a
    -> (Maybe (Graph a), Maybe (Graph a))
resolveLink (Link source target) graph =
    (resolve source graph, resolve target graph)

-- | Apply a link to a graph.
applyLink :: Graph a -> Link a -> Graph a
applyLink g l@(Link source target) = 
    case resolve source g of
        Just g' -> update target (g' >->) g
        Nothing -> g

-- | Flip the direction of a link.
reverseLink :: Link a -> Link a
reverseLink (Link s t) = Link t s
