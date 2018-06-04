{-# OPTIONS_GHC -Wall #-}

{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = (oneOrMore p) <|> (pure [])

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = f <$> p <*> (zeroOrMore p)
  where f x y = [x] ++ y

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = f <$> oneOrMore (satisfy isAlpha) <*> zeroOrMore (satisfy isAlphaNum)
  where f x y = x ++ y

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
data Atom = N Integer | I Ident
  deriving Show

-- An S-expression is either an atom, or a list of S-expressions.
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show

parseAtom :: Parser Atom
parseAtom = (f <$> posInt) <|> (g <$> ident)
  where f x = N x
        g y = I y

parseSExpr :: Parser SExpr
parseSExpr = (A <$> (spaces *> parseAtom <* spaces ) )
             <|>
             (Comb <$> (spaces *> (char '(') *>
                        (zeroOrMore parseSExpr)
                         <* (char ')') <* spaces ))
