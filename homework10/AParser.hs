{-# GHC_OPTIONS -Wall #-}
{-# LANGUAGE InstanceSigs #-}
{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------
first :: (a -> b) -> (a,c) -> (b,c)
first g (x, y) = (g x, y)

firstMaybe :: (a->b) -> Maybe (a,c) -> Maybe (b,c)
firstMaybe f = fmap (first f)

-- firstMaybeWithString :: (String -> (a->b))
--                      -> (String -> Maybe(a,c))
--                      -> (String -> Maybe(b, c))

instance Functor Parser where
  fmap :: (a->b) -> Parser a -> Parser b
  fmap f p = Parser h
      where h s = fmap (first f) ((runParser p) s)
-- fmap f p@(Parser g) = Parser h
--    where h (runParser.Parser $ g $ s) = firstMaybe f ((runParser p) s)

instance Applicative Parser where
  pure a = Parser g
    where g "" = Nothing
          g s = Just(a, s)

  (<*>) :: Parser (a->b) -> Parser a -> Parser b
  pab <*> pb = Parser h
    where h s = case (runParser pab) s of
            Nothing -> Nothing
            Just(f, sRest) -> fmap (first f) ((runParser pb) sRest)

-- ex3
-- You should implement
-- each of the following exercises using the Applicative interface to put
-- together simpler parsers into more complex ones. Do not implement
-- them using the low-level definition of a Parser! In other words, pretend
-- that you do not have access to the Parser constructor or even
-- know how the Parser type is defined.
abParser :: Parser (Char, Char)
abParser = (\x y ->(x,y)) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\_ _ -> ()) <$> (char 'a') <*> (char 'b')

intPair :: Parser [Integer]
intPair = (\x _ y -> [x,y]) <$> posInt <*> (char ' ') <*> posInt

-- ex4
instance Alternative Parser where
  empty = Parser p
   where p _ = Nothing
-- empty  _ = Nothing
  p1 <|> p2 = Parser p
    where p s = (runParser p1 s) <|> (runParser p2 s)

--ex5
intOrUppercase :: Parser ()
intOrUppercase = ((\ _ -> ()) <$> posInt)
                 <|> ((\ _ -> ()) <$> (satisfy isUpper))

-- work for lecture 11
(*>)       :: Applicative f => f a -> f b -> f b
(*>) fa fb = (\ _ y -> y ) <$>  fa <*> fb

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA g = h
  where h [] = pure []
        h (x:xs) = liftA2 (\ x y -> [x] ++ y) (g x) (h xs)

sequenceA'  :: Applicative f => [f a] -> f [a]
sequenceA' = mapA f
  where f = id

-- replicateA :: Applicative f => Int -> f a -> f [a]
-- replicateA = liftA2 replicate
