{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module OpenGames.Engine.Repeated where

import OpenGames.Engine.OpticClass
import OpenGames.Engine.MonadicOptics
import OpenGames.Engine.OpenGames
import OpenGames.Engine.TLL
import OpenGames.Engine.MC (fromFunctions)

-- Operator for finite horizon *Markov* games
-- Currently specialised to 2 player games plus 1 logger
repeatFinite :: (Monad m)
             => Int 
             -> OpenGame (MonadicOptic m) (MonadicContext m) '[s1, s2] '[m r1, m r2, m r3] x x' x x'
             -> OpenGame (MonadicOptic m) (MonadicContext m) '[s1, s2] '[m [r1], m [r2], m [r3]] x x' x x'
repeatFinite 0 g = reindex (\_ -> Nil) 
                           (\_ _ -> pure [] ::- pure [] ::- pure [] ::- Nil) 
                           (fromFunctions id id)
repeatFinite n g = reindex (\(s1 ::- s2 ::- Nil) -> s1 ::- s2 ::- s1 ::- s2 ::- Nil)
                           (\_ (r1 ::- r2 ::- r3 ::- rs1 ::- rs2 ::- rs3 ::- Nil) -> f r1 rs1 ::- f r2 rs2 ::- f r3 rs3 ::- Nil) 
                           (g >>> repeatFinite (n - 1) g)
  where f r rs = do {x <- r; xs <- rs; pure (x : xs)}
