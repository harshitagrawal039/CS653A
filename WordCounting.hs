{--
 WordCounting.hs
 Q- Write a program to read a file and :
 (a) print number of words/lines in it
 (b) list all the unique words in the file
 (c) find frequency count of all words in a file (after removing some noise words like -"a", "an", "the")

 --}
--module WordCounting where 
module Main where

-- countWords count the number of words in a string
countWords :: String -> Int
countWords xs = aux xs True where
 	aux :: String -> Bool -> Int
 	aux [] _ = 0
 	aux (x:xs) flag
 			| x `elem` " \t\n"	= aux xs True
 			| flag				= 1 + aux xs False
 			| otherwise			= aux xs False

-- splitInWords splits a string into words
splitInWords :: String -> [String]
splitInWords xs = aux xs "" where
	aux :: String -> String -> [String]
	aux [] curr
			| curr /="" 	= [curr]
			| otherwise		= []
	aux (x:xs) curr
			| (x `elem` " \t\n") && (curr /= "")	= curr : aux xs ""
			| x `elem` " \t\n"						= aux xs ""
			| otherwise								= aux xs $ curr++[x]

-- (x `count` xs) gives number of times x is present in list xs
count :: Eq a => a -> [a] -> Int
count _ [] = 0
count x xs = foldl  (\a b -> a + if b==x then 1 else 0) 0 xs

-- (isElemTill a l 5) will check if a is an element of list l within first 5 elements
isElemTill :: Eq a => a -> [a] -> Int -> Bool
isElemTill _ [] _	= False
isElemTill x (l:ls) i
			| i <= 0	= False
			| x==l 		= True
			| otherwise	= isElemTill x ls $ i-1

-- listWords provides list of all unique words
listWords :: String -> [String]
listWords = aux . splitInWords where
	aux :: Eq a => [a] -> [a]
	aux (x:xs)	= x : aux (filter (/=x) xs)
	aux []		= []

-- listFreq gives frequency count of all unique words in a list
listFreq :: String -> [(String, Int)]
listFreq = aux . splitInWords where
	aux :: Eq a => [a] -> [(a, Int)]
	aux l@(x:xs)	= (x, x `count` l ) : aux (filter (/=x) xs)
	aux []			= []

noiseWords = ["the", "The", "a", "A", "an", "An"]

-- removeNoise removes all noise words from a list
removeNoise :: [String] -> [String]
removeNoise = filter (\x -> not $ x `elem` noiseWords)

-- listFreq2 gives frequency count of all unique words except noise words in a list
listFreq2 :: String -> [(String, Int)]
listFreq2 = aux . removeNoise . splitInWords where
	aux :: Eq a => [a] -> [(a, Int)]
	aux l@(x:xs)	= (x, x `count` l ) : aux (filter (/=x) xs)
	aux []			= []

main = do
	print "Type your sentence:"
	content <- getContents
	print $ "Number of words: " ++ show (countWords content)
	print $ "List of all unique words:"
	print $ listWords content
	print "Frequencies:"
	print $ listFreq2 content
	print "Thankyou! Bye..."

