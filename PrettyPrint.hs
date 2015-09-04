--PrettyPrint.hs

module PrettyPrint where

type  Row = [String]
type Table = [Row]
type Width = [Int]

{--}
padRow :: Row -> Width -> (Width, Row)
padRow [] _ = ([], [])
padRow (r:rs) (w:ws) = (a:c, b:d)
	where
		(a,b) = padSingle r w
		(c,d) = padRow rs ws 

padSingle :: String -> Int -> (Int, String)
padSingle word size = (length word, word ++ (replicate diff ' '))
	where
		diff = size - length word
--}

{--
-- Some helper functions
pad :: Row -> Width ->[(Int, String)]
pad t w1 = zipWith padSingle t w1
	where
		padSingle :: String -> Int -> (Int, String)
		padSingle word size = (length word, word ++ (replicate diff ' '))
						where
							diff = size - length word

splitRecordList :: [(a,b)] -> ([a], [b])
splitRecordList [] = ([], [])
splitRecordList ((l,r):xs) = (l:l1, r:r1)
	where
		(l1, r1) = splitRecordList xs
--}

-- An auxiliary function
aux :: Table -> Width -> Width -> (Width, Table)
aux [] _ w2 = (w2, [])
aux (t:ts) w1 w2 = (w, t1) 
	where
		(ws, t2) = aux ts w1 w2
		--(wr, tr) = splitRecordList $ pad t w1
		(wr, tr) = padRow t w1
		w = zipWith max ws wr
		t1 = tr:t2

-- prettyPrint function
prettyPrint :: Table -> Table
prettyPrint t = t1
	where
		(w, t1) = aux t w (replicate rowsize 0)
		rowsize = length row
		(row:_) = t

table = [["hi", "hello", "hey"], ["ram", "she", "sundar"], ["aloo", "karela", "ab"]]
a = aux table [1,1,1] [0,0,0]
b = prettyPrint table