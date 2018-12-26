module Optimall.Definition.Build
( linkedTemplate
) where

import qualified Data.List as List
import qualified Data.Map as Map
import Optimall.Definition.Hierarchy
import Optimall.Definition.Template
import Optimall.Definition.Link

-- | Allows new templates to be specified in terms of old templates
-- and the links between them.
data LinkedTemplate a = LinkedTemplate String
    (Map.Map String (Template a)) [Link]

-- | Create a linked template from a list of key-pair
-- values and a list of links.
linkedTemplate :: String -> [(String, Template a)]
    -> [Link] -> Template a
linkedTemplate name keys links =
    let lt = LinkedTemplate name (Map.fromList keys) links
    in build lt

-- | Build a linked template into a fully-fledged
-- template object.
build :: LinkedTemplate a -> Template a
build lt = Template (buildName lt) (buildSchema lt)
    (buildTemplateCheck lt) (buildShapeCheck lt)
    (buildApply lt) (buildDerivatives lt)

-- | Build the name value for a linked template.
buildName :: LinkedTemplate a -> String
buildName (LinkedTemplate n _ _) = n

-- | Build a schema from a linked template.
buildSchema :: LinkedTemplate a -> Schema
buildSchema (LinkedTemplate n m _) = Keyed (Map.map (schema) (m)) n

-- | Build an auto-generated template check from
-- a linked template.
buildTemplateCheck :: LinkedTemplate a -> TemplateCheck a
buildTemplateCheck (LinkedTemplate _ m _) (Keyed m' _) =
    Map.foldr (++) [] errors
    where
        errors = Map.mapWithKey (checkSubgraph) m
        checkSubgraph key template
            | Map.member key m' =
                if metadata (m' Map.! key) == template
                then (templateCheck template) (m' Map.! key)
                else ["the subgraph " ++ key ++
                    " must have the type " ++ name template]
            | otherwise =
                ["the graph must have a subgraph named " ++ key]
buildTemplateCheck _ _ = error "The input must be a keyed graph."

-- | Build an auto-generated shape check from
-- a linked template.
buildShapeCheck :: LinkedTemplate a -> ShapeCheck a
buildShapeCheck (LinkedTemplate _ subgraphs links) g@(Keyed m' _) =
    (Map.foldr (++) [] graphErrors) ++ linkErrors
    where
        graphErrors = Map.mapWithKey (checkSubgraph) m'
        checkSubgraph key subgraph =
            (shapeCheck . metadata $ subgraph) (m' Map.! key)
        linkErrors = filter (not . null) . map (checkLink) $ links
        checkLink (Link src tgt) =
            if shapesMatch (g /../ src) (g /../ tgt)
            then "" else
                let fmt = List.intercalate "/"
                in "one or more shapes do not match between "
                     ++ fmt src ++ " and " ++ fmt tgt

-- | Determine whether the shapes of the nodes in
-- two graphs match.
shapesMatch :: Hierarchy [Int] (Template a)
    -> Hierarchy [Int] (Template a) -> Bool
shapesMatch (Unit s _) (Unit s' _) = s == s'
shapesMatch (Keyed m _) (Keyed m' _) =
    allMatch $ Map.foldrWithKey (checkKey) [] m
    where
        checkKey key val acc = (val, (m' Map.! key)) : acc
        allMatch = all (\(a, b) -> shapesMatch a b)
shapesMatch (Ordered l _) (Ordered l' _) =
    allMatch (zip l l') 
    where
        allMatch = all (\(a, b) -> shapesMatch a b)
shapesMatch _ _ = False

-- | Build an apply function for a linked template.
buildApply = error "NYI."

-- | Build a derivatives template for a linked template.
buildDerivatives = error "NYI."
