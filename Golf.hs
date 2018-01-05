{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List (drop,tails)
import Control.Applicative

everyn :: Int -> [a] -> [a]
everyn _ [] = []
everyn n (x:xs) = x:(everyn n $ drop n xs)

{-
which is error, since take 10 $ everynReverse x [1..] will no results
-}
everynReverse' :: Int -> [a] -> [a]
everynReverse' x li = reverse $ everyn x $ reverse li

-- this the correct version, just drop first n elem
everySkipn :: Int -> [a] ->[a]
everySkipn n li = everyn n $ drop n li

everySkipn' :: [a] -> Int -> [a]
everySkipn' li n = everySkipn n li


skips :: [a] -> [[a]]
skips [] = []
skips li = map (everySkipn' li) [0..((length li) - 1)]


-- ex2
{-
Exercise 2 Local maxima
A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it. For
example, in the list [2,3,4,1,5], the only local maximum is 4, since
it is greater than the elements immediately before and after it (3 and
1). 5 is not a local maximum since there is no element that comes
after it.
Write a function
localMaxima :: [Integer] -> [Integer]
which finds all the local maxima in the input list and returns them in
order. For example:
localMaxima [2,9,5,6,1] == [9,6]
localMaxima [2,3,4,1,5] == [4]
localMaxima [1,2,3,4,5] == []
-}
localMaxima :: Ord a => [a] -> [a]
localMaxima =  foldr g [] . init . tails . (zip <*> tail) 
   where
     g ((a,b):(_,c):_) r | a<b && b>c = b:r
     g  _              r              =   r

head3LBL :: Ord a => [a] -> Maybe a
head3LBL l = let a = head l
                 b = head.tail $ l
                 c = head.tail.tail $ l
                 in
               case b > a && b > c of
                 True -> Just b
                 _ -> Nothing

localMaxima' :: [Integer] -> [Maybe Integer]
localMaxima' li = let lli = init.init.init.tails $ li
                      in
                    foldl (\ acc x ->  (head3LBL x):acc) [] lli

localMaxima'' :: [Maybe Integer] -> [Integer]
localMaxima'' li = foldl (\ acc x -> case x of Just a -> a:acc) [] (filter (\ x -> case x of Nothing -> False; otherwise -> True) li)

-- para :: (a -> ([a], b) -> b) -> b -> [a] ->  b
-- para f b (a:as) = f a (as, para f b as)
-- para _ b []     = b
-- localMaxima''' :: Ord a => [a] -> [a]
-- localMaxima''' =  foldr para [] . init . tails . (zip <*> tail) 

{-
Exercise 3 Histogram
For this task, write a function
histogram :: [Integer] -> String
which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers). Your output must exactly match the output shown in the
examples below.
histogram [1,1,1,5] ==
 *
 *
 *   *
==========
0123456789
histogram [1,4,5,4,6,6,3,4,2,4,9] ==
    *
    *
    * *
 ******  *
==========
0123456789
Important note: If you type something like histogram [3,5] at
the ghci prompt, you should see something like this:
" * * \n==========\n0123456789\n"
This is a textual representation of the String output, including \n
escape sequences to indicate newline characters. To actually visualize
the histogram as in the examples above, use putStr, for example,
putStr (histogram [3,5]).
-}
countTimes:: [Int] -> [(Integer, Integer)]

countTimes = foldr (\x times ->
                      (take x times) ++
                      [(fst (times!!x), (snd (times!!x)+1))] ++
                      (drop (x+1) times))
             (zip [0..] $ take 10 $ repeat 0)

printCount :: [Int] -> String
printCount = foldr (\ x pOut ->
                      (replicate x ' ')
                      ++ "*" ++
                      (replicate (10-x-1)  ' ')
                      ++ "\n" ++ pOut) []

-- histogram :: [Integer] -> String
-- histogram = countTimes.printCount
