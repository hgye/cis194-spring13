{-# GHC_OPTIONS -Wall#-}

module Party where

import Data.Monoid
import Data.Tree
import Employee

-- ex1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp n f) gl@(GL l fgl) = GL (e:l) (f+fgl)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL l1 f1) (GL l2 f2) =
    GL (l1++l2) (f1+f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 > f2 = gl1
  | otherwise = gl2

-- ex2
treeFold ::  Monoid b => b -> (b -> Forest a -> b) -> Tree a -> b
treeFold e _ (Node r []) = e
treeFold e f (Node root forest) = f  $ map (treeFold e f) forest

-- --combineGLs :: Employee -> [GuestList] -> GuestList
