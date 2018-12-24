# Optimall
A Haskell library (WIP) for creating and optimising modular, declarative computation graphs for tensor inputs.  The main purpose of this will be to rapidly prototype neural network architectures and ways of training them.

The main theme of Optimall is that the data contained within computation graphs is separated from the logic that describes how the results should be calculated.  The idea behind this is that models can be defined in a more declarative function and moved around quickly and easily, while swapping in different parameters or inputs as and when.  It should be noted that the library is intended to be used for rapid prototyping and so is not optimised for speed.

## Data types

There are four main data types that are of particular importance, the first of which is a `Tensor` typeclass, representing a standard n-dimensional array and declaring some of the standard mathematical operations that can be applied to them (mapping, summing, dot product, etc.).  The other three data types are explained in more detail below.
