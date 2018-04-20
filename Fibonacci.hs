{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

-- ex1
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib xx = fib (xx-1) + fib (xx-2)

fibs' :: [Integer] -> [Integer]
fibs' xs = fib (head xs) : fibs' (tail xs)

fibs1 :: [Integer]
--fibs1 = fibs' [0..]
fibs1 = g [0..] where
  g xs = fib (head xs) : g (tail xs)

-- ex2
fibs2 :: [Integer]
-- fibs2 = 1:1:zipWith (+) fibs2 (tail fibs2)
fibs2 = 0:1: (zipWith (+) <*> tail) fibs2

-- ex3
data Stream a = Cons a (Stream a)
-- infixr 5 `Cons`

streamToList :: Stream a -> [a]
streamToList (Cons a b) = a:(streamToList b)

instance Show a => Show (Stream a) where
  show a = show.(take 20).streamToList $ a

-- ex4
streamRepeat :: a -> Stream a
streamRepeat x1 = Cons x1 (streamRepeat x1)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a b) = Cons (f a) (streamMap f b)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f a = Cons (f a) (streamFromSeed f (f a))

-- ex5
nats :: Stream Integer
nats = streamFromSeed (+1) (-1)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons a b) (Cons c d) = Cons a $ Cons c (interleaveStreams b d)

ruler :: Stream Integer
ruler = Cons 0 $ interleaveStreams (streamMap (+1) ruler) (streamRepeat 0)

-- ex6
x::Stream Integer
x = Cons 0 $ Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
   fromInteger n = Cons n $ streamRepeat 0
   negate (Cons a s) = Cons (-a) $ negate s

   (+) (Cons a aL) (Cons b bL) = Cons (a+b) ((+) aL bL)

   (*) (Cons a as) (Cons b bs) =
     Cons (a*b) $  (streamMap (*a) bs) + (as * (Cons b bs))

instance Fractional (Stream Integer) where
   (/) (Cons a as) (Cons b bs) = q
     where q = Cons (div a b)  (streamMap (`div` b) (as - q*bs))


fibs3 :: Stream Integer
fibs3 =
  x/(1-x-x^2)

-- ex7
data Matrix = Matrix Integer Integer Integer Integer
  deriving Show

instance Num Matrix  where
  (*) (Matrix a1 a2 a3 a4) (Matrix b1 b2 b3 b4) =
    Matrix (a1*b1+a2*b3) (a1*b2+a2*b4)
           (a3*b1+a4*b3) (a3*b2+a4*b4)

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = f $ y^n
  where y = Matrix 1 1 1 0
        f (Matrix _ a _ _) = a 
