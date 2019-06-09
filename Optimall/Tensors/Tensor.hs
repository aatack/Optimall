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
    
    -- | Map over the last n dimensions of the tensor.
    partialMap :: Int -> (t a -> t b) -> t a -> t b

    -- | Map every element of the tensor according to a transfer function.
    tensorMap :: (t a -> t b) -> t a -> t b
    tensorMap = partialMap 0

    -- | Zip the last n dimensions of two tensors.
    partialZip :: Int -> t a -> t b -> t (a, b)

    -- | Zip the elements of two tensors together.
    tensorZip :: t a -> t b -> t (a, b)
    tensorZip = partialZip 0

    -- | Flatten a tensor from the -nth dimension onwards.
    partialFlatten :: Int -> t a -> t a
    partialFlatten n = partialMap n (flatten)

    -- | Flatten the tensor into a rank-one tensor.
    flatten :: t a -> t a
    flatten = partialFlatten 0

    -- | Reduce a tensor dimension by dimension to a rank-zero tensor
    -- by applying the same function multiple times.
    tensorReduce :: (t a -> t a) -> t a -> t a
    tensorReduce f tensor
        | rank tensor == 1 = f tensor
        | rank tensor == 0 = tensor
        | otherwise        = tensorReduce f $ partialMap 1 f tensor

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

    -- | Take slices of each dimension given the lower (inclusive) and
    -- upper (exclusive) bounds.
    slices :: t a -> [(Int, Int)] -> t a

    -- | Take a slice from the first dimension of the tensor.
    slice :: t a -> (Int, Int) -> t a
    slice tensor bounds = slices tensor [bounds]

    {- Updating -}

    -- | Update the value at the given index.
    update :: t a -> [Int] -> t a -> t a

    -- | Update the value at the root index given.
    updateRoot :: t a -> [Int] -> a -> t a
    updateRoot tensor i x = update tensor i (wrap x)

    {- Construction -}

    -- | Wrap a singular value in a tensor.
    wrap :: a -> t a

    -- | Wrap a vector of values in a rank-one tensor.
    wrapVector :: Vector.Vector a -> t a

    -- | Wrap a list of values in a rank-one tensor.
    wrapList :: [a] -> t a

    -- | Repeat a value n times to form a rank-one tensor.
    repeat :: Int -> a -> t a

    -- | Wrap a vector of tensors into a tensor one rank higher.
    stackVector :: Vector.Vector (t a) -> t a

    -- | Wrap a list of tensors into a tensor one rank higher.
    stackList :: [t a] -> t a

    {- Mathematical Operations -}

    {- Dimension Rearrangement -}

    {- Common Tensors -}

    -- | Create a tensor which spans a one-dimensional space with the
    -- given number of steps, evenly spaced.
    linearOneDimensional :: (Float, Float) -> Int -> t Float
    linearOneDimensional (lower, upper) steps = wrapList . resize . toFloat $ indices
        where
            indices = [0 .. steps - 1]
            toFloat = map (fromIntegral)
            resize = map (* stepSize)
            stepSize = (upper - lower) / (fromIntegral steps)
