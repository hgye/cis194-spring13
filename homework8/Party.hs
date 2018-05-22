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
-- catamophosim, homework3
treeFold ::  ( a -> [b] -> b) -> Tree a -> b
--treeFold  f (Node root forest) =  f root $ map (treeFold f) forest
treeFold f = go where
  go (Node root forest) = f root (map go forest)

treeSize :: Tree a -> Integer
treeSize = treeFold (\ _ list -> foldr (+) 1 list )

treeSum :: Tree Integer -> Integer
treeSum  = treeFold (\ x list -> foldr (+) x list)

treeFlatten :: Tree a -> [a]
treeFlatten = treeFold (\x list -> foldr (++) [x] list)

-- treeDepth :: Tree a -> Integer
-- treeDepth = treeFold 1 (\ _ list -> case list of
--                           [] -> 1
--                           _  -> foldr (+) 0 list)

combineGLs :: Employee -> [GuestList] -> GuestList
combineGLs e = foldr moreFun (GL [e] (empFun e)) 

-- ex3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e lst = (glwithboss, glwithoutboss) where
  -- glwithoutboss = foldr (<>) (GL [] 0) $ map snd lst
  glwithoutboss = moreFun
    (foldr moreFun (GL [] 0) $ map snd lst)
    (foldr (<>) (GL [] 0) $ map snd lst)
  glwithboss = moreFun
    (foldr moreFun (GL [e] (empFun e)) $ map fst lst)
    (foldr (<>) (GL []  0) $ map fst lst)
  -- glwithoutboss = combineGLs e $ map snd lst

-- ex4
maxFun :: Tree Employee -> GuestList
maxFun t
  | gl1 > gl2 = gl1
  | otherwise = gl2
  where
    (gl1, gl2) = treeFold nextLevel t
-- maxFun' using combineGLs as fold function, which only could return max fun Employee as GL [E] f
-- why? because at last, a node of tree is N (E) [],
-- leaf is empty, could only return GL [E] f or GL [] 0
-- GL could never be such as GL (E1+E2) f1+f2
maxFun' :: Tree Employee -> GuestList
maxFun' = treeFold combineGLs

-- this is the version could directlly get the maxFun of (tree e)
combineGLs' :: Employee -> [GuestList] -> GuestList
combineGLs' e gl = moreFun (foldr moreFun (GL [e] (empFun e)) gl)
 (foldr (<>) (GL [] 0) gl)

maxFun'' :: Tree Employee -> GuestList
maxFun'' = treeFold combineGLs'

tree1 :: Tree Employee
tree1 = Node (Emp "Stan" 2)
  [Node (Emp "Bob" 18) 
   [ Node (Emp "Joe" 5) [],
     Node (Emp "John" 3) []
   ],
   Node (Emp "x" 12) 
   [
     Node (Emp "x" 7) [],
     Node (Emp "y" 17) []
   ]
  ]

-- ex5
putEmployeeLn :: Employee -> IO ()
putEmployeeLn e = putStrLn (empName e)

putGuestList :: GuestList -> IO ()
putGuestList (GL l f) = do
  putStrLn ("Total fun:" ++ show f)
  mapM_ putEmployeeLn l

main :: IO ()
main = do
  readFile "company.txt" >>= putGuestList.maxFun.read

