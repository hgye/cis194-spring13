{-# OPTIONS_GHC -Wall #-}

module Party where

-- import Data.Monoid
import Data.Tree
import Employee

-- ex1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) (GL l fgl) = GL (e:l) (f+fgl)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 > f2 = gl1
  | otherwise = gl2

-- ex2
treeFold ::  b -> ( a -> [b] -> b) -> Tree a -> b
treeFold e f (Node root forest) =  f root $ map (treeFold e f) forest

treeSize :: Tree a -> Integer
treeSize = treeFold 1 (\ _ list -> foldr (+) 1 list )

treeSum :: Tree Integer -> Integer
treeSum  = treeFold 0 (\ x list -> foldr (+) x list)

-- --combineGLs :: Employee -> [GuestList] -> GuestList
 
