{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import StackVM
import Parser

{-
Exercise 1
Write Version 1 of the calculator: an evaluator for ExprT, with the
signature
eval :: ExprT -> Integer
For example ,eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20.
-}
eval :: ExprT -> Integer
eval e = case e of
  Lit i -> i
  ExprT.Add e1 e2 -> (+) (eval e1) (eval e2)
  ExprT.Mul e1 e2 -> (*) (eval e1) (eval e2)


{-
Exercise 2
The UI department has internalized the focus group data and is
ready to synergize with you. They have developed the front-facing
user-interface: a parser that handles the textual representation of the
selected language. They have sent you the module Parser.hs, which
exports parseExp, a parser for arithmetic expressions. If you pass
the constructors of ExprT to it as arguments, it will convert Strings
representing arithmetic expressions into values of type ExprT. For
example:
*Calc> parseExp Lit Add Mul "(2+3)*4"
Just (Mul (Add (Lit 2) (Lit 3)) (Lit 4))
*Calc> parseExp Lit Add Mul "2+3*4"
Just (Add (Lit 2) (Mul (Lit 3) (Lit 4)))
*Calc> parseExp Lit Add Mul "2+3*"
Nothing
Leverage the assets of the UI team to implement the value-added
function
evalStr :: String -> Maybe Integer
which evaluates arithmetic expressions given as a String, producing
Nothing for inputs which are not well-formed expressions, and
Just n for well-formed inputs that evaluate to n.
-}
evalM :: Maybe ExprT -> Maybe Integer
evalM e  = case e of
  Nothing -> Nothing
  Just x -> Just $ eval x

evalStr :: String -> Maybe Integer
evalStr  = evalM.parseExp Lit ExprT.Add ExprT.Mul

{-
Exercise 3
Good news! Early customer feedback indicates that people really
do love the interface! Unfortunately, there seems to be some disagreement
over exactly how the calculator should go about its calculating
business. The problem the software department (i.e. you) has is that
while ExprT is nice, it is also rather inflexible, which makes catering
to diverse demographics a bit clumsy. You decide to abstract away
the properties of ExprT with a type class.
Create a type class called Expr with three methods called lit, add,
and mul which parallel the constructors of ExprT. Make an instance of
Expr for the ExprT type, in such a way that
mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
== Mul (Add (Lit 2) (Lit 3)) (Lit 4)
Think carefully about what types lit, add, and mul should have. It
may be helpful to consider the types of the ExprT constructors, which
you can find out by typing (for example)
*Calc> :t Lit
at the ghci prompt.
Remark. Take a look at the type of the foregoing example expression:
*Calc> :t mul (add (lit 2) (lit 3)) (lit 4)
Expr a => a
What does this mean? The expression mul (add (lit 2) (lit 3)) (lit 4)
has any type which is an instance of the Expr type class. So writing it
by itself is ambiguous: GHC doesn’t know what concrete type you
want to use, so it doesn’t know which implementations of mul, add,
and lit to pick.
One way to resolve the ambiguity is by giving an explicit type
signature, as in the above example. Another way is by using such an
expression as part of some larger expression so that the context in
which it is used determines the type. For example, we may write a
function reify as follows:
reify :: ExprT -> ExprT
reify = id
To the untrained eye it may look like reify does no actual work!
But its real purpose is to constrain the type of its argument to ExprT.
Now we can write things like
reify $ mul (add (lit 2) (lit 3)) (lit 4)
at the ghci prompt.
-}
class Expr a where
  lit :: Integral b => b->a
  add :: a->a->a
  mul :: a->a->a

instance Expr ExprT where
  lit a = Lit (toInteger a)
  add e1 e2 = ExprT.Add e1 e2
  mul e1 e2 = ExprT.Mul e1 e2

reify :: ExprT -> ExprT
reify = id

instance Expr Bool where
  lit a = case (toInteger a)>0 of
    True -> True
    False -> False
  add e1 e2 = (||) e1 e2
  mul e1 e2 = (&&) e1 e2

newtype MinMax = MinMax Integer deriving (Eq, Show)

evalMinMax :: MinMax -> Integer
evalMinMax (MinMax a) = a
-- evalMinMax e = case e of
--   MinMax a -> a

instance Expr MinMax where
  lit a = MinMax (toInteger a)
  add e1 e2 = lit $ max (evalMinMax e1) (evalMinMax e2)
  mul e1 e2 = lit $ min (evalMinMax e1) (evalMinMax e2)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

evalMod7 :: Mod7 -> Integer
evalMod7 (Mod7 a) = a
--evalMod7 e = case e of
--  Mod7 a -> a

instance Expr Mod7 where
  lit a = Mod7  ((\x -> mod x 7) (toInteger a))
  add e1 e2 = lit $ mod ((evalMod7 e1)+(evalMod7 e2)) 7
  mul e1 e2 = lit $ mod ((evalMod7 e1)*(evalMod7 e2)) 7

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"
-- testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7


-- {- ex5 -}
instance Expr Program where
  lit a = PushI (toInteger  1a)
  add e1 e2 = stackVM e1 ++ stackVM e2 ++ [StackVM.Add]
  mul e1 e2 = stackVM e1 ++ stackVM e2 ++ [StackVM.Mul]
