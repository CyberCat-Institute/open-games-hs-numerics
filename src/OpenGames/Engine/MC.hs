{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module OpenGames.Engine.MC where

-- Adapted from https://github.com/philipp-zahn/open-games-engine/blob/monte-carlo-monad-bayes/src/OpenGames/Engine/BayesianMC.hs
-- but (1) with the state hack removed for efficiency,
-- and (2) with a decision operator that just forwards payoffs to an external optimiser

import OpenGames.Engine.OpenGames
import OpenGames.Engine.MonadicOptics
import OpenGames.Engine.TLL

fromFunctions :: (Monad m) 
               => (s -> a) -> (b -> t)
               -> OpenGame (MonadicOptic m) (MonadicContext m) '[] '[] s t a b
fromFunctions f g = OpenGame {
    play = \Nil -> let v s = pure ((), f s)
                       u () b = pure (g b)
                    in MonadicOptic v u
  , evaluate = \Nil _ -> Nil
}

decisionMC :: (Monad m)
           => OpenGame (MonadicOptic m)
                       (MonadicContext m)
                       '[x -> m y]
                       '[m r]
                       x () y r
decisionMC = OpenGame {
    play = \(f ::- Nil) 
      -> let v x = do {y <- f x; pure ((), y)}
             u () _ = pure ()
          in MonadicOptic v u
  , evaluate = \(f ::- Nil) (MonadicContext h k)
      -> let result = do {(z, x) <- h; y <- f x; k z y}
          in result ::- Nil
}
