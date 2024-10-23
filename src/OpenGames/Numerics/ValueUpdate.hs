{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module OpenGames.Numerics.ValueUpdate where

import Data.Ix
import Data.Array.MArray
import Data.Array.IO

import OpenGames.Engine.OpenGames
import OpenGames.Engine.MonadicOptics
import OpenGames.Engine.TLL

type Values x = IOUArray x Double

-- Compute the target value for 1 stage of Bellman iteration at a single state
-- TODO just sketching
bellman :: (Ix y) 
         => OpenGame (MonadicOptic IO) (MonadicContext IO) '[strategy] '[costrategy] x Double y Double
         -> strategy
         -> Values y
         -> x
         -> IO Double
bellman g strategy values x = pullback (play g (strategy ::- Nil)) (readArray values) x

-- Alternative formulation of Bellman update using evaluate rather than coplay
bellman' :: (Ix y)
         => OpenGame (MonadicOptic IO) (MonadicContext IO) '[strategy] '[IO Double] x x' y Double
         -> strategy
         -> Values y
         -> x
         -> IO Double
bellman' g strategy values x 
  = let k () = readArray values
        result ::- Nil = evaluate g (strategy ::- Nil) (MonadicContext (pure ((), x)) k)
     in result

convex :: Double -> Double -> Double -> Double
convex learningRate newValue oldValue = learningRate*newValue + (1 - learningRate)*oldValue

-- TODO just sketching
-- Note, this reads the array twice, but in general they are at different states:
-- V(current state) := learningRate*target(V(next state)) + (1 - learningRate)*V(currentState)
-- Of course one of them can be skipped if we fix a learning rate of 1, a la classical control
-- TODO The function modifyArray' from array >= 0.5.6.0 skips an unnecessary bounds check (I think)
-- but I can't figure out how to make stack build it
updateValue :: (Ix x)
            => OpenGame (MonadicOptic IO) (MonadicContext IO) '[strategy] '[costrategy] x Double x Double
            -> strategy
            -> Double
            -> Values x
            -> x
            -> IO ()
updateValue g strategy learningRate values x = do {
  currentValue <- readArray values x;
  target <- bellman g strategy values x; -- TODO profile vs !target here
  writeArray values x (convex learningRate target currentValue)
  -- TODO import modifyArray' and use this instead:
  -- modifyArray' values x (convex learningRate target)
}

-- Alternative using bellman'
updateValue' :: (Ix x)
             => OpenGame (MonadicOptic IO) (MonadicContext IO) '[strategy] '[IO Double] x x' x Double
             -> strategy
             -> Double
             -> Values x
             -> x
             -> IO ()
updateValue' g strategy learningRate values x = do {
  currentValue <- readArray values x;
  target <- bellman' g strategy values x; -- TODO profile vs !target here
  writeArray values x (convex learningRate target currentValue)
}
