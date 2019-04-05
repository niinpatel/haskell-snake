module Utils where

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1, f a2)

toInt :: Float -> Int
toInt = floor

toFloats :: (Int, Int) -> (Float, Float)
toFloats tuple = mapTuple fromIntegral tuple

toInts :: (Float, Float) -> (Int, Int)
toInts tuple = mapTuple toInt tuple
