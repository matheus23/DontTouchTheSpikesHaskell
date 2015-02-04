module FRP.Behaviour where

data Behaviour e v = Behaviour { value :: v, runEvent :: e -> Behaviour e v }

constant :: v -> Behaviour e v
constant value = Behaviour value $ const $ constant value

setValue :: v -> Behaviour e v -> Behaviour e v
setValue newValue (Behaviour value runEvent) = Behaviour newValue runEvent

remember :: e -> Behaviour e e
remember initialValue = Behaviour initialValue remember

fold :: s -> (e -> s -> s) -> Behaviour e s
fold initialState stateF = Behaviour initialState step
  where step input = fold (stateF input initialState) stateF

filterE :: (e -> Bool) -> Behaviour e v -> Behaviour e v
filterE predicate (Behaviour value runEvent) = Behaviour value run
  where run event = if predicate event
          then filterE predicate $ runEvent event
          else filterE predicate $ Behaviour value runEvent

filterMapE :: (e -> Maybe e') -> Behaviour e' v -> Behaviour e v
filterMapE predicate behaviour = Behaviour (value behaviour) run
  where run event = case (predicate event) of
                      Just event' -> filterMapE predicate $ runEvent behaviour event'
                      Nothing     -> filterMapE predicate behaviour

merge :: (a -> b -> c) -> Behaviour e a -> Behaviour e b -> Behaviour e c
merge f behaviourA behaviourB = Behaviour (f (value behaviourA) (value behaviourB)) step
  where step event = merge f (runEvent behaviourA event) (runEvent behaviourB event)

mapValue :: (v -> v') -> Behaviour e v -> Behaviour e v'
mapValue f (Behaviour value runEvent) = Behaviour (f value) $ \e -> mapValue f $ runEvent e

mapEvent :: (e -> e') -> Behaviour e' v -> Behaviour e v
mapEvent f behaviour = Behaviour (value behaviour) step
  where step event = mapEvent f $ runEvent behaviour $ f event
