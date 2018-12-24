import qualified Data.Map as Map

data Node a = Node NodeType [Int] (Maybe a)
data NodeType = Input | Output | Parameter Int | Custom String

data Hierarchy a b = Unit b a
                   | Keyed b (Map.Map String (Hierarchy a b))
                   | Ordered b [Hierarchy a b]

type Graph a = Hierarchy (Node a) (Template a)

data Template a = Template { schema :: Hierarchy NodeType String
                           , templateCheck :: TemplateCheck a
                           , shapeCheck :: ShapeCheck a
                           , apply :: Graph a -> Graph a
                           , derivatives :: String -> Template a
                           }

type TemplateCheck a = Hierarchy NodeType (Template a) -> [String]
type ShapeCheck a = Hierarchy [Int] (Template a) -> [String]
