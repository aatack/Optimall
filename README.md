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

The first parameter is the type of the node, which is defined as

```
data NodeType = Input | Output | Parameter Int | Custom String
```

Any node can have any type, and the type should be ignored when checking the validity of computation graphs; it is mostly to serve as documentation but some utility functions are also provided, for example extracting all inputs from the graph.  The integer for the `Parameter` type denotes the parameter group that node belongs to.  This may come in handy when defining networks that have more than one set of parameters that need to be trained independently, such as GANs.

A node also contains the shape of its data in the form of a `[Int]`.  It should be noted that this does not allow for nodes to be created that accept tensors of an arbitrary size: the intended way of accomplishing this is to define a function that wraps a tensor in a customised node (more on this later).

The last argument is a `Maybe a` that actually stores the value of the node.  If the value is `Nothing` then this just means that the value has either not been provided yet - as may be the case for inputs - has not been calculated yet or that an error occurred during its computation, or is otherwise undefined.
