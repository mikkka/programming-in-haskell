module Generig where
  mapMM :: Monad m => (a -> m b) -> [a] -> m [b]
  mapMM f []      = pure []
  mapMM f (x:xs)  = f x >>= 
                          (\y -> mapMM f xs
                            >>= (\ys -> pure (y:ys)))

