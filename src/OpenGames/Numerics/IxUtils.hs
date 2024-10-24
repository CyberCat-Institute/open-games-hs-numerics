-- The type Data.Array.IO.IOUArray (for mutable, unboxed, strict arrays) 
-- cannot directly store eg. enumeration types, or pairs of things
-- But it can store Ints, so we can extend it to any type in Ix

module OpenGames.Numerics.IxUtils where

import Data.Array.MArray
import Data.Array.IO

readArrayIx :: (Ix x, Ix y) => (y, y) -> IOUArray x Int -> x -> IO y
readArrayIx bounds array x = do {
  n <- readArray array x;
  pure (range bounds !! n) -- TODO Ix uses lists here, is this a bottleneck in practice?
}

writeArrayIx :: (Ix x, Ix y) => (y, y) -> IOUArray x Int -> x -> y -> IO ()
writeArrayIx bounds array x y = writeArray array x (index bounds y)
