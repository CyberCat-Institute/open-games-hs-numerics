{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module OpenGames.Engine.MC where

-- Adapted from https://github.com/philipp-zahn/open-games-engine/blob/monte-carlo-monad-bayes/src/OpenGames/Engine/BayesianMC.hs
-- but (1) with the state hack removed for efficiency,
-- and (2) with a decision operator that just forwards payoffs to an external optimiser

import OpenGames.Engine.OpenGames
import OpenGames.Engine.MonadicOptics
import OpenGames.Engine.TLL

-- TODO change evaluate to send n samples
decisionMC :: (Monad m)
           => Int
           -> OpenGame (MonadicOptic m)
                       (MonadicContext m)
                       '[x -> m y]
                       '[m [r]]
                       x () y r
decisionMC numSamples = OpenGame {
    play = \(f ::- Nil) 
      -> let v x = do {y <- f x; pure ((), y)}
             u () _ = pure ()
          in MonadicOptic v u
  , evaluate = \(f ::- Nil) (MonadicContext h k)
      -> let sample = do {(z, x) <- h; y <- f x; k z y}
             result = sequence (replicate numSamples sample)
          in result ::- Nil
}
