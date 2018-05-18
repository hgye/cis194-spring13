{-# OPTIONS_GHC -Wall #-}

module Party where

import Data.Monoid
import Data.Tree
import Employee

-- ex1
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp _ f) (GL l fgl) = GL (e:l) (f+fgl)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL el1 _) gl2@(GL el2 _)
  | f1 > f2 = gl1
  | otherwise = gl2
  where f1 = foldr (\ e lst -> glCons e lst) (GL [] 0) el1
        f2 = foldr (\ e lst -> glCons e lst) (GL [] 0) el2

-- ex2
treeFold ::  b -> ( a -> [b] -> b) -> Tree a -> b
treeFold e f (Node root forest) =  f root $ map (treeFold e f) forest

treeSize :: Tree a -> Integer
treeSize = treeFold 1 (\ _ list -> foldr (+) 1 list )

treeSum :: Tree Integer -> Integer
treeSum  = treeFold 0 (\ x list -> foldr (+) x list)

treeFlatten :: Tree a -> [a]
treeFlatten = treeFold [] (\x list -> foldr (++) [x] list)

-- treeDepth :: Tree a -> Integer
-- treeDepth = treeFold 1 (\ _ list -> case list of
--                           [] -> 1
--                           _  -> foldr (+) 0 list)

combineGLs :: Employee -> [GuestList] -> GuestList
--combineGLs e [] = glCons e (GL [] 0)
combineGLs _ [] = GL [] 0
combineGLs e (x:xs) =  moreFun (glCons e (GL [] 0))
  $ moreFun x (combineGLs e xs)
   --True -> x
   --_ -> combineGLs e xs


-- ex3
nextLevel :: Employee -> [(GuestList, GuestList)]
  -> (GuestList, GuestList)
nextLevel e@(Emp _ f) [] = (GL [e] f, GL [] 0)
nextLevel e ((x,y) : xs)
  | y > snd (nextLevel  e xs)= (x,y)
  | otherwise = nextLevel e xs

-- ex4
maxFun :: Tree Employee -> (GuestList, GuestList)
--maxFun t@(n@(Node e@(Emp _ f) ) _) treeFold (GL [e] f, GL [] 0) nextLevel t
maxFun t@(Node (Emp e f) _) = treeFold (GL [Emp e f] f, GL [] 0) nextLevel t

maxFun' :: Tree Employee -> GuestList
maxFun' = treeFold (GL [] 0) combineGLs

tree1 :: Tree Employee
tree1 = Node (Emp "Stan" 2)
  [Node (Emp "Bob" 1)
   [ Node (Emp "Joe" 5) [],
     Node (Emp "John" 3) []
   ],
   Node (Emp "x" 12)
   [
     Node (Emp "x" 7) [],
     Node (Emp "y" 17) []
   ]
  ]
