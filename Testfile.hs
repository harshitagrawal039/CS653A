module Testfile where

	-- Aliasing
	type Person = (String, Int)

	print :: Person -> String
	print (a,b) = "My name is "++a++" and my age is "++ show b ++" . Nice to meet you!"

	--class Eq where 
	--	(==) :: a->a->Bool

	-- defining user types
	data Tree a = Empty
				| Node (Tree a) a (Tree a)
				deriving Show

	-- defining (Tree a) as an instance of Eq class
	instance Eq a => Eq (Tree a) where
		(==) Empty Empty = True
		(==) (Node l1 a1 r1) (Node l2 a2 r2) = (l1==l2) && (a1==a2) && (r1==r2)
		(==) _ _ = False
	
	--deriving some classes
	data Rainbow = Blue
				| Violet
				| Red
				deriving (Eq, Show, Ord)

	a = Red
	b = Violet

	-- type inference of Haskell
	quicksort [] 		= []
	quicksort (x:xs) 	= quicksort lt ++ [x] ++ quicksort rt
		where
		(lt, rt) = partition (<x) xs
		partition:: (a->Bool)->[a]->([a],[a])
		partition _ _ = ([], [])
	-- Now type :t quicksort