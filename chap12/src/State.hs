module State where
  type State = Int

  newtype ST a = S(State -> (a,State))

  app :: ST a -> State -> (a, State)
  app (S st) = st

  instance Functor ST where
    fmap g st = S(\s -> let (x, s') = app st s in (g x, s'))

  instance Applicative ST where
    pure x = S(\s -> (x, s))

    stf <*> stx = S(\s ->
      let (f, s')  = app stf s
          (x, s'') = app stx s' in (f x, s''))

  instance Monad ST where
    st >>= f = S(\s -> let (x, s') = app st s in app (f x) s')