module Parser where

import Control.Applicative hiding (many)

data Result a	= ParseError
				| Ok a String

data Parser a = Parser {runParser :: String -> Result a}

instance Functor Result where
	fmap f ParseError = ParseError
	fmap f (Ok a str) = Ok (f a) str

instance Functor Parser where
	fmap f (Parser pa) = Parser $ fmap f . pa

instance Applicative Parser where
	pure a = Parser (Ok a)
	(Parser pf) <*> (Parser pa) = Parser g
		where
			g x0 =	case pf x0 of
						ParseError -> ParseError
						Ok f rest  -> fmap f $ pa rest

instance Monad Parser where
	return = pure
	(>>=) (Parser pa) f = Parser pb
		where
			pb x0 = case pa x0 of
						ParseError -> ParseError
						Ok a rest  -> runParser (f a) rest

-- Now define some helper functions
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser g
	where
		g []	= ParseError
		g (x:xs)	| f x			= Ok x xs
					| otherwise 	= ParseError

(<|>) :: Parser a -> Parser a -> Parser a
(Parser pa1) <|> (Parser pa2) = Parser pa
	where
		pa x = case pa1 x of
			ParseError	-> pa2 x
			res 		-> res

many :: Parser a -> Parser [a]
many (Parser pa) = Parser g
	where
		g x0 = case pa x0 of
			ParseError	-> Ok [] x0
			Ok a x		-> Ok (a:resta) restx
				where
					(Ok resta restx) = (runParser $ many (Parser pa)) x


