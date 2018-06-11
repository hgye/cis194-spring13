{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Applicative
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom


threeInts :: Rand StdGen (Int, Int, Int)
threeInts =
  getRandom >>= \i1 ->
  getRandom >>= \i2 ->
  getRandom >>= \i3 ->
  return (i1,i2,i3)

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
  deriving Show


-- ex2
getDiceList :: Int -> Rand StdGen [DieValue]
getDiceList n
  | n == 0 = die >>= \_ -> return []
  | otherwise = (\ x y -> x : y) <$> die
                <*>  (getDiceList (n-1))

combineDiceList :: Rand StdGen [DieValue] -> Rand StdGen [DieValue] ->
  Rand StdGen Battlefield
combineDiceList dl1 dl2 = f <$> dl1 <*> dl2
  where f [] y = Battlefield 0 (length y)
        f x [] = Battlefield (length x) 0
        f (x:xs) (y:ys) = case x > y of
          True -> g (Battlefield 1 0) (f xs ys)
          False -> g (Battlefield 0 1) (f xs ys)
          where g p1 p2 = Battlefield (attackers p1 + attackers p2) (defenders p1 + defenders p2)

getFighter:: Int -> Int -> Int
getFighter y x
  | y > x = x - 1
  | otherwise = y - 1

battle :: Battlefield -> Rand StdGen Battlefield
battle b = combineDiceList ((reverse . sort) <$> (getDiceList a))
           ((reverse . sort) <$> (getDiceList d))
  where a = getFighter 4 (attackers b)
        d = getFighter 3 (defenders b)

