module Optimall.Definition.Build
( linkedTemplate
) where

import qualified Data.Map as Map
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
buildSchema = error "NYI."

-- | Build an auto-generated template check from
-- a linked template.
buildTemplateCheck = error "NYI."

-- | Build an auto-generated shape check from
-- a linked template.
buildShapeCheck = error "NYI."

-- | Build an apply function for a linked template.
buildApply = error "NYI."

-- | Build a derivatives template for a linked template.
buildDerivatives = error "NYI."

