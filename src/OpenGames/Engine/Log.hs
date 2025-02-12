{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module OpenGames.Engine.Log where

import OpenGames.Engine.OpenGames
import OpenGames.Engine.TLL
import OpenGames.Engine.MonadicOptics

logger :: (Monad m) => OpenGame (MonadicOptic m) (MonadicContext m) '[] '[m x] () () () x
logger = OpenGame {
    play = \Nil -> MonadicOptic (\() -> pure ((), ())) (\() _ -> pure ())
  , evaluate = \Nil (MonadicContext h k) -> do {(z, ()) <- h; k z ()} ::- Nil
}
