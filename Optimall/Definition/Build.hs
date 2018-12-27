module Optimall.Definition.Build
( linkedTemplate
) where

import qualified Data.List as List
import qualified Data.Map as Map
import Optimall.Definition.Hierarchy
import Optimall.Definition.Node
import Optimall.Definition.Template
import Optimall.Definition.Link

-- | Allows new templates to be specified in terms of old templates
-- and the links between them.
data LinkedTemplate a = LinkedTemplate String
    (Map.Map String (Template a)) [Link a]

-- | Create a linked template from a list of key-pair
-- values and a list of links.
linkedTemplate :: String -> [(String, Template a)]
    -> [Link a] -> Template a
linkedTemplate name keys links =
    let lt = LinkedTemplate name (Map.fromList keys) links
    in build lt

-- | Build a linked template into a fully-fledged
-- template object.
build :: LinkedTemplate a -> Template a
build lt = Template (buildName lt) (buildSchema lt)
    (buildCheck lt) (buildApply lt) (buildDerivatives lt)

-- | Build the name value for a linked template.
buildName :: LinkedTemplate a -> String
buildName (LinkedTemplate n _ _) = n

-- | Build a schema from a linked template.
buildSchema :: LinkedTemplate a -> Schema
buildSchema (LinkedTemplate n m _) = Keyed (Map.map (schema) (m)) n

-- | Build a function that checks whether or not an arbitrary graph
-- can conform to the requirements of a linked template.
buildCheck :: LinkedTemplate a -> Graph a -> [String]
buildCheck (LinkedTemplate _ keys links) g@(Keyed keys' _) =
    let keyErrors = Map.foldr (++) []
            (Map.mapWithKey (checkKey) keys)
        linkErrors = foldr (++) [] (map (checkLink) links)
    in keyErrors ++ linkErrors
    where
        checkKey k template = case Map.lookup k keys' of
            Just subgraph -> check template subgraph
            Nothing       -> ["key " ++ k ++ " should be present"]
        checkLink l =
            let (source, target) = resolveLink l g
            in case source of
                Nothing      -> ["link source does not exist"]
                Just source' -> case target of
                    Nothing      -> ["link target does not exist"]
                    Just target' -> checkLinkTemplate source' target'
        checkLinkTemplate source target =
            let templateErrors = check (metadata target) source
            in if not . null $ templateErrors
                then templateErrors
                else checkLinkShapes source target
        checkLinkShapes source target = if shapesMatch source target
            then [] else ["linked node shapes do not match"]
buildCheck _ _ = ["graph types do not match (should be keyed)"]

-- | Determine whether or not all the nodes contained
-- within two graphs have matching shapes.
shapesMatch :: Graph a -> Graph a -> Bool
shapesMatch (Unit n _) (Unit n' _) = shape n == shape n'
shapesMatch (Keyed m _) (Keyed m' _) =
    all (checkKey) (Map.keys m')
    where
        checkKey k = shapesMatch (m Map.! k) (m' Map.! k)
shapesMatch (Ordered l _) (Ordered l' _) =
    all (checkPair) (zip l l')
    where
        checkPair (a, b) = shapesMatch a b
shapesMatch _ _ = False

-- | Build an apply function for a linked template.
buildApply = error "NYI."

-- | Build a derivatives template for a linked template.
buildDerivatives = error "NYI."
