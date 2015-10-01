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
put s = State (\_ -> ((), s))

stateOf :: (a,s) -> s
stateOf = snd

modify :: (s -> s) -> State s ()
modify f = State $ \x -> ((), f x)