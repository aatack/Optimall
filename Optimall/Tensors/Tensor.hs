module Optimall.Tensors.Tensor
( Tensor
) where

import qualified Data.Vector as Vector

class Tensor t where

    {- Tensor Data -}

    -- | Gives the shape of the tensor.
    shape :: t a -> [Int]

    -- | Gives the size of the first dimension of the tensor.
    firstDimensionSize :: t a -> Int
    firstDimensionSize = head . shape

    -- | Gives the number of elements in the tensor.
    dimensionality :: t a -> Int
    dimensionality = (foldr (*) 1) . shape

    -- | Gives the rank of the tensor.
    rank :: t a -> Int
    rank = length . shape

    {- Tensor Transformations -}

    -- | Map every element of the tensor according to a transfer function.
    tensorMap :: (a -> b) -> t a -> t b

    -- | Zip the elements of two tensors together.
    tensorZip :: t a -> t b -> t (a, b)

    -- | Map over the last n dimensions of the tensor.
    incompleteMap :: Int -> (a -> b) -> t a -> t b

    -- | Zip the last n dimensions of two tensors.
    incompleteZip :: Int -> t a -> t b -> t (a, b)

    -- | Flatten the tensor into a single vector.
    flatten :: t a -> Vector.Vector a

    {- Indexing -}

    -- | Index a sub-tensor by following a given index path.
    index :: t a -> [Int] -> t a

    -- | Try to extract a value from a zero-rank tensor.
    extract :: t a -> Maybe a

    -- | Extract a value from a zero-rank tensor without checking
    -- it is of zero rank first.
    unsafeExtract :: t a -> a
    unsafeExtract t = case extract t of
        Just x  -> x
        Nothing -> error "Tried to extract value from a non-zero rank Tensor"

    {- Updating -}

    {- Mathematical Operations -}

    {- Dimension Rearrangement -}
