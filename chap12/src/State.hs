module State where
  type State = Int

  newtype ST a = S(State -> (a,State))

  app :: ST a -> State -> (a, State)
  app (S st) x = st x

