module Parser where

import Control.Applicative

data Result a	= ParseError
				| Ok a String

data Parser a = Parser {runParser :: String -> Result a}

instance Functor Result where
	fmap f ParseError = ParseError
	fmap f (Ok a str) = Ok (f a) str

instance Functor Parser where
	fmap f (Parser pa) = Parser (\x -> fmap f $ pa x)

instance Applicative Parser where
	pure a = Parser (\x -> Ok a x)
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

