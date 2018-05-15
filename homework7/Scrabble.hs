{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty = 0
  mappend = (+)

class Scored a where
  score :: a -> Score

instance Scored Score where
  score = id

instance Scored a => Scored (a,b) where
  score = score.fst

instance Scored Char where
--score :: Char -> Score
  score c
    | c `elem` ['D', 'G', 'd', 'g'] = 2
    | c `elem` [ 'B' , 'C' , 'M' , 'P' , 'b' , 'c' , 'm' , 'p'] = 3
    | c `elem` ['F' , 'H' , 'V' , 'Y' , 'f' , 'h' , 'v' , 'y' ] = 4
    | c `elem` ['K' , 'k'] = 5
    | c `elem` ['J' , 'X' , 'j' , 'x']= 8
    | c `elem` ['Q' , 'Z' , 'q' , 'z'] = 10
    | c `elem` ['A' , 'E' , 'I' , 'L' , 'O' , 'R' , 'S' , 'T' , 'U',
                'a' , 'e' , 'i' , 'l' , 'o' , 'r' , 's' , 't' , 'u'] = 1
    | otherwise = 0


scorestring :: String -> Score
scorestring [] = Score 0
scorestring (x:xs) = score x <> scorestring xs
