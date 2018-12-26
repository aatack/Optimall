module Optimall.Definition.Link
( Link
, (-->)
, (/->)
, (-/>)
, (//>)
) where

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
