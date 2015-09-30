module MoveKnight where

{--
type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]  
moveKnight (c,r) = filter onBoard  
    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
    ]  
    where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]  

{--
in3 :: KnightPos -> [KnightPos]  
in3 start = do   
    first <- moveKnight start  
    second <- moveKnight first  
    moveKnight second  
--}

in3 :: KnightPos -> [KnightPos]
in3 start = return (start) >>= moveKnight >>= moveKnight >>= moveKnight

inN :: Int -> KnightPos -> [KnightPos]
inN 0 start = [start]
inN n start = do
	first <- moveKnight start
	inN (n-1) first

canReachIn3 :: KnightPos -> KnightPos -> Bool  
canReachIn3 start end = end `elem` in3 start

{--
data Result a	= Yes a
				| No

instance Monad Result where
	return a = Yes a
	(Yes a) >>= f = f a
	No >>= f = No
--}

canReachIn3' :: KnightPos -> KnightPos -> [(KnightPos, KnightPos)]
canReachIn3' start end = 
						do
							first <- moveKnight start
							second <- moveKnight first
							third <- moveKnight second
							if third == end then
								return (first,second)
							else
								[]

canReachInN :: Int -> KnightPos -> KnightPos -> [[KnightPos]]
canReachInN 0 start end  = if start==end then [[]]
							else []
canReachInN n start end = do
							first <- moveKnight start
							rest <- canReachInN (n-1) first end
							if (rest == [] && n/=1)
								then []
							else
								[first:rest]
--}
newtype Writer w a = Writer { runWriter :: (a, w) } deriving (Eq, Show)

instance Functor (Writer w) where
	fmap f (Writer (x,l)) = Writer (f x, l)

instance (Monoid w) => Applicative (Writer w) where
	pure x = Writer (x, mempty)
	Writer (f,l1) <*> Writer (x, l2) = Writer (f x, l1 `mappend` l2)

instance (Monoid w) => Monad (Writer w) where  
    return x = Writer (x, mempty)  
    (Writer (x,v)) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')  

type Result a = Writer [a] a
type KnightPos = (Int,Int)

moveKnight :: KnightPos -> [Result KnightPos] 
moveKnight (c,r) = do 
	x <- filter (\(c,r) -> c `elem` [1..8] && r `elem` [1..8]) 
		    [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
		    ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
		    ]  --where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]  
	return (Writer (x,[(c,r)]))

inN :: Int -> KnightPos -> [Result KnightPos]
inN 0 start = return (return start)
inN n start = do
	Writer (x,v) <- moveKnight start
	Writer (x1,v1) <- (inN (n-1) x)
	return (Writer (x1, v `mappend` v1))

canReachInN :: Int -> KnightPos -> KnightPos -> [[KnightPos]]
canReachInN n start dest = map (snd . runWriter) (filter (isDest dest) $ inN n start)
	where
		isDest dest (Writer (x,_)) = dest == x