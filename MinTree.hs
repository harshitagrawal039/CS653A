--MinTree.hs

module Mintree where

data Tree a = Empty
			| Node (Tree a) a (Tree a)
			deriving Show

data Infinity a = Infinity
				| Val a
				deriving (Show, Eq)

instance (Ord a) => Ord (Infinity a) where
	compare Infinity Infinity	= EQ
	compare Infinity _			= GT
	compare _ Infinity			= LT
	compare (Val a) (Val b)	= compare a b

null :: Tree a -> Bool
null Empty	= True
null _		= False

empty :: Tree a
empty = Empty

singleton :: a -> Tree a
singleton a = Node empty a empty

--infinity = read "Infinity" :: Int

-- An auxiliary function
aux :: (Ord a) => Tree a -> a -> a -> (a, Tree a)
aux Empty _ d = (d, empty)
aux (Node lt x1 rt) x d = (m, Node lt1 x rt1)
	where
		(mlt, lt1) = aux lt x d
		(mrt, rt1) = aux rt x d
		m = minimum [mlt, d, mrt, x1]

-- minTree function
minTree :: (Ord a) => Tree (Infinity a) -> Tree (Infinity a)
minTree t = t1
	where
		(x,t1) = aux t x Infinity

{--
a = Node (singleton $ Val 8) (Val 5) (Node (singleton $ Val 2) (Val 1) empty)
--}

