module Optimall.Tensors.Recursive where

import qualified Optimall.Tensors.Tensor as Tensor
import qualified Data.Vector as Vector

data RecursiveTensor a = Scalar a | Vector (Vector.Vector (RecursiveTensor a))
