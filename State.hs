module State where

import Control.Applicative

data State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
	fmap f (State h) = State g where
		g s0 = (f a, s1) where
			(a, s1) = h s0

instance Applicative (State s) where
	pure a = State (\x -> (a, x))
	(State sf) <*> (State sa) = State g where
		g s0 = (f a, s2) where
			(f, s1) = sf s0
			(a, s2) = sa s1

instance Monad (State s) where
	return = pure
	(State sa) >>= f = State g where
		g s0 = (runState . f) a s1  where
			(a, s1) = sa s0

-- Some helper functions
get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put s = State $ const ((), s)

stateOf :: (a,s) -> s
stateOf = snd

modify :: (s -> s) -> State s ()
modify f = do
	s <- get
	put $ f s

modifyAfter :: (s -> s) -> State s a -> State s a
modifyAfter f sa = do
	a <- sa
	modify f
	return a

evalState :: State s a -> s -> a
evalState sa = fst . runState sa

execState :: State s a -> s -> s
execState sa = stateOf . runState sa

