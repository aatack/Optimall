# Optimall

A Haskell library (WIP) for creating and optimising modular, declarative computation graphs for tensor inputs.  The main purpose of this will be to rapidly prototype neural network architectures and ways of training them.

The main theme of Optimall is that the data contained within computation graphs is separated from the logic that describes how the results should be calculated.  The idea behind this is that models can be defined in a more declarative function and moved around quickly and easily, while swapping in different parameters or inputs as and when.  It should be noted that the library is intended to be used for rapid prototyping and so is not optimised for speed.

## Data types

There are four main data types that are of particular importance, the first of which is a `Tensor` typeclass, representing a standard n-dimensional array and declaring some of the standard mathematical operations that can be applied to them (mapping, summing, dot product, etc.).  The other three data types are explained in more detail below.

### Node

Data that make up part of a computation graph are wrapped up in a `Node`.  While strictly speaking any data type can be wrapped up, it is assumed that generally speaking that type will be an instance of `Tensor` and so some additional arguments are given that provide extra information about the tensor.

```
data Node a = Node NodeType [Int] (Maybe a)
```

The first parameter is the type of the node, which is defined as:

```
data NodeType = Input | Output | Parameter Int | Custom String
```

Any node can have any type, and the type should be ignored when checking the validity of computation graphs; it is mostly to serve as documentation but some utility functions are also provided, for example extracting all inputs from the graph.  The integer for the `Parameter` type denotes the parameter group that node belongs to.  This may come in handy when defining networks that have more than one set of parameters that need to be trained independently, such as GANs.

A node also contains the shape of its data in the form of a `[Int]`.  It should be noted that this does not allow for nodes to be created that accept tensors of an arbitrary size: the intended way of accomplishing this is to define a function that wraps a tensor in a customised node (more on this later).

The last argument is a `Maybe a` that actually stores the value of the node.  If the value is `Nothing` then this just means that the value has either not been provided yet - as may be the case for inputs - has not been calculated yet or that an error occurred during its computation, or is otherwise undefined.

### Hierarchy & Graph

A hierarchy can be thought of as a bit like a JSON object that contains some metadata for the whole object.

```
import qualified Data.Map as Map
data Hierarchy a b = Unit b a
                   | Keyed b (Map.Map String (Hierarchy a b))
                   | Ordered b [Hierarchy a b]
```

The type argument `b` is the metadata stored when the hierarchy branches into unordered key-value pairs or an ordered list respectively, while the type `a` is the data type stored at the leaves.  A `Hierarchy` is rarely used on its own, and most of the interactions are done via a specific instance of it, specifically a `Graph`:

```
type Graph a = Hierarchy (Node a) (Template a)
```

Templates will be defined in the next section, but the important part is that a graph consists of a JSON-like object with `Node`s at its leaves, with a `Template` at each level.

A graph representing a single layer of a MLP might look something like this (in psuedocode):

```
mlpLayer -> { "input"   -> Node Input [5] Nothing
            , "weights" -> Node (Parameter 0) [2, 5] Nothing
            , "biases"  -> Node (Parameter 0) [2] Nothing
            , "output"  -> Node Output [2] Nothing
            }
```

### Template

So we have a way of representing the data in a graph, and a way of representing the structure of the graph.  A template's job is to describe how all this information is put together to calculate the values of the graph outputs given its inputs, and how to calculate the derivatives of the graph's nodes with respect to each other.  It also contains some utility functions for checking that it can operate on a particular instance of a `Graph`.

```
data Template a = Template { schema :: Hierarchy NodeType String
                           , templateCheck :: TemplateCheck a
                           , shapeCheck :: ShapeCheck a
                           , apply :: Graph a -> Graph a
                           , derivatives :: String -> Template a
                           }

type TemplateCheck a = Hierarchy NodeType (Template a) -> [String]
type ShapeCheck a = Hierarchy [Int] (Template a) -> [String]
```

The first part is the schema, which describes in a user-friendly way which nodes and subgraphs should be contained in a graph that can be operated on by the template.  A function, `templateCheck`, is provided, which checks that the graph is of the right type (`Unit`, `Keyed` or `Ordered`), contains the right keys if necessary, and checks that any nodes or subgraphs contained in it also conform to the expected template types.  The template check returns a list of any errors it encountered; if there are no errors, it should be safe to run the shape check.

A shape check simply checks that the shapes of all the nodes are consistent with the operations that will be applied to them.  For example, the MLP layer above might have shape checks that return error strings if the bias does not have the same shape as the output, or if the weights do not have a shape that can be multiplied by the input.

Calling `apply` on a graph will populate the values of the nodes in the template, assuming that the values of the inputs and parameters are already known.  Any nodes whose values cannot be calculated due to insufficient information will be left as `Nothing`.  The power of this comes from the fact that it allows one graph to be carried around as a partially-complete version that can then have multiple inputs passed through it, which can be used to perform the same computation multiple times in things like convolutional networks or LSTMs.

Finally, the most important part of the template is its derivatives function.  When given a string represnting the key or index of a node or subgraph in the template's graph (or an empty string if the graph is a `Unit`), this function returns a completely new template.  The template should apply only to `Keyed` instances of `Graph`, and as part of its template check it should check that the keys `"values"`, `"externalDerivatives"`, and `"internalDerivatives"` are present.

The subgraph contained in the `"values"` key should have the same form as the original template, and it will contain the values of each node or subgraph in that graph once a feedforward pass has been done.  The `"externalDerivatives"` key will point to a single node, whose tensor represents the derivative of some arbitrary node, which may be external to the graph, with respect to the pivot node (the pivot node is the node that was specified by the string in the call to `derivatives`).  Then, the `internalDerivatives` contains a subgraph whose form is the same as `"values"` but whose nodes' values represent the derivative of the aforementioned external node with respect to each node in the graph.

In practice, this will almost always mean calculating the derivatives of each node with respect to the pivot node, and multiplying the external derivatives by this new tensor.  The derivatives function is only defined in this way so that, in special cases that would otherwise require the multiplication of two sparse tensors, the internal derivatives can be provided directly as a function of the external derivatives.

One problem with this is that defining a new template type could become very arduous, since each template will need to define another kind of template for each of the nodes it contains, each of which will need to define another template for each of its nodes, and so on.  This will be overcome by leveraging Haskell's ability to lazily compute values, and then defining a small number of very simple templates explicitly (which won't be too hard since their derivatives will quickly become zero).  More complicated templates can then be defined in terms of existing templates and links between their nodes, at which point functions can do the heavy lifting needed to recursively define their `apply` and `derivatives` functions.

While this may seem like a very long way of doing things, once a few basic templates and the functions for creating `apply` and `derivatives` instances have been implemented, creating new arbitrarily complex templates should be very simple for the end user.  Another nice property of doing it this way is that, since the derivatives of the nodes of a template will themselves have a template, we can calculate the *n*th derivative of things without any extra work on the user's end.  This should afford the user more flexibility when it comes to working with Optimall - for example, 2nd derivatives can be used in optimisers, or the output derivatives can be used as a regularisation variable, and so on.
