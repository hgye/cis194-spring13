{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

-- data JoinListBasic a = Empty
--   | Single a
--   | Append (JoinListBasic a) (JoinListBasic a)

data JoinList m a = Empty
  | Single m a
  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- ex1
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) x y = Append ((tag x) <> (tag y)) x y

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

-- ex2
indexJ' :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a
indexJ' i jl = jlToList jl  !!? i

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

indexJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> Maybe a
indexJ _ Empty = Nothing
indexJ i (Single _ a) =
  case i of
    0 -> Just a
    _ -> Nothing
indexJ i jj@(Append _ jl1 jl2) =
  case (getSize.size.tag $ jj) < i of
    True -> Nothing
    False ->
      case (getSize.size.tag $ jl1) > i of
        True -> indexJ i jl1
        False -> indexJ (i - (getSize.size.tag $ jl1)) jl2


dropJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ i jl@(Single _ _) =
  case i > 1 of
    True -> Empty
    False -> jl
dropJ i jl@(Append m jl1 jl2) =
  case (getSize.size.tag $ jl) > i of
    True -> Empty
    False -> case (getSize.size.tag $ jl1) > i of
      True -> (Append m (dropJ i jl1) jl2)
      False -> dropJ (i - (getSize.size.tag $ jl1)) jl2

takeJ :: (Sized b, Monoid b) =>
  Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ i jl@(Single _ _) =
  case i > 0 of
    True -> jl
    False -> Empty
takeJ i jl@(Append m jl1 jl2) =
  let f = getSize.size.tag in
    case (f jl) < i of
      True -> jl
      False -> case (f jl1) > i of
        True -> takeJ i jl1
        False -> (Append m jl1 (takeJ (i - (f jl1)) jl2))

-- ex3
scoreLine :: String -> JoinList Score String
scoreLine [] = Empty
scoreLine s = (Single (scorestring s) s)

-- ex4
instance Buffer (JoinList (Score, Size) String) where
  toString jl = case jl of 
    Empty -> ""
    (Single _ s) -> s
    (Append m jl1 jl2) -> toString jl1 ++ toString jl2
  fromString s
    | [] = Empty
    | otherwise = Single (scorestring s, Size 1) s
  -- line = indexJ
  -- replaceLine n l b = l
  -- numLines = fst.tag
  -- value = snd.tag
