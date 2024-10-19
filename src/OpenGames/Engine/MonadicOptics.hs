{-# LANGUAGE GADTs, MultiParamTypeClasses #-}

module OpenGames.Engine.MonadicOptics where

import OpenGames.Engine.OpticClass

-- TODO profile whether these go faster with existentials or nested functions
data MonadicOptic m s t a b where
  MonadicOptic :: (s -> m (z, a)) -> (z -> b -> m t) -> MonadicOptic m s t a b
-- newtype MonadicOptic m s t a b = MonadicOptic (s -> m (a, b -> m t))

data MonadicContext m s t a b where
  MonadicContext :: m (z, s) -> (z -> a -> m b) -> MonadicContext m s t a b
-- newtype MonadicContext m s t a b = MonadicContext (m (s, a -> m b))

instance (Monad m) => Optic (MonadicOptic m) where
  lens v u = MonadicOptic (\s -> return (s, v s)) (\s b -> return (u s b))
  (>>>>) (MonadicOptic v1 u1) (MonadicOptic v2 u2) = MonadicOptic v u
    where v s = do {(z1, a) <- v1 s; (z2, p) <- v2 a; return ((z1, z2), p)}
          u (z1, z2) q = do {b <- u2 z2 q; u1 z1 b}
  (&&&&) (MonadicOptic v1 u1) (MonadicOptic v2 u2) = MonadicOptic v u
    where v (s1, s2) = do {(z1, a1) <- v1 s1; (z2, a2) <- v2 s2; return ((z1, z2), (a1, a2))}
          u (z1, z2) (b1, b2) = do {t1 <- u1 z1 b1; t2 <- u2 z2 b2; return (t1, t2)}
  (++++) (MonadicOptic v1 u1) (MonadicOptic v2 u2) = MonadicOptic v u
    where v (Left s1)  = do {(z1, a1) <- v1 s1; return (Left z1, Left a1)}
          v (Right s2) = do {(z2, a2) <- v2 s2; return (Right z2, Right a2)}
          u (Left z1) b  = u1 z1 b
          u (Right z2) b = u2 z2 b

instance (Monad m) => Precontext (MonadicContext m) where
  void = MonadicContext (pure ((), ())) (\() () -> return ())

instance (Monad m) => Context (MonadicContext m) (MonadicOptic m) where
  cmap (MonadicOptic v1 u1) (MonadicOptic v2 u2) (MonadicContext h k)
    = let h' = do {(z, s) <- h; (_, s') <- v1 s; return (z, s')}
          k' z a = do {(z', a') <- v2 a; b' <- k z a'; u2 z' b'}
       in MonadicContext h' k'
  (//) (MonadicOptic v u) (MonadicContext h k)
    = let h' = do {(z, (s1, s2)) <- h; return ((z, s1), s2)}
          k' (z, s1) a2 = do {(_, a1) <- v s1; (_, b2) <- k z (a1, a2); return b2}
       in MonadicContext h' k'
  (\\) (MonadicOptic v u) (MonadicContext h k)
    = let h' = do {(z, (s1, s2)) <- h; return ((z, s2), s1)}
          k' (z, s2) a1 = do {(_, a2) <- v s2; (b1, _) <- k z (a1, a2); return b1}
       in MonadicContext h' k'
