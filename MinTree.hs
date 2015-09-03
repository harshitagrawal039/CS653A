--MinTree.hs

module Mintree where

data Tree a = Empty
			| Node (Tree a) a (Tree a)
			deriving Show

null :: Tree a -> Bool
null Empty	= True
null _		= False

empty :: Tree a
empty = Empty

singleton :: a -> Tree a
singleton a = Node empty a empty

infinity = read "Infinity" :: Int

-- An auxiliary function
aux :: (Ord a, Num a) => Tree a -> a -> a -> (a, Tree a)
aux Empty _ d = (d, empty)
aux (Node lt x1 rt) x d = (m, Node lt1 x rt1)
	where
		(mlt, lt1) = aux lt x d
		(mrt, rt1) = aux rt x d
		m = minimum [mlt, d, mrt, x1]

-- minTree function
minTree :: (Ord a, Num a) => Tree a -> Tree a
minTree t = t1
	where
		(x,t1) = aux t x 1000

