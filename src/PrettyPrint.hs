{-# LANGUAGE FlexibleInstances #-}
module PrettyPrint where

import qualified Data.List as L

class PrettyPrint a where
  prettyPrint :: a -> String

instance PrettyPrint String where
  prettyPrint = id
  
newtype Print s = Print [s]
  deriving (Eq, Show)

instance PrettyPrint s => PrettyPrint (Print s) where
  prettyPrint (Print ls) = concatMap prettyPrint ls

instance Semigroup (Print s) where
  Print m1 <> Print m2 = Print (m1 <> m2)

instance Monoid (Print s) where
  mempty = empty
  (Print p1) `mappend` (Print p2) = Print $ p1 ++ p2

instance Functor Print where
  fmap f (Print ls) = Print (fmap f ls)

empty :: Print s 
empty = Print []

add :: s -> Print s -> Print s
add s (Print ps) = Print (s:ps)

append :: [s] -> Print s -> Print s 
append = mappend . Print 

between :: Print s -> s -> s -> Print s -> Print s
between (Print str) start end pnt = Print ((start:str) ++ [end]) `mappend` pnt

parens :: Print String -> Print String -> Print String 
parens doc = between doc "(" ")"

intercalate :: [[s]] -> [s] -> Print [s] -> Print [s]
intercalate ss sep = add $ L.intercalate sep ss 

addSpace :: Print String -> Print String 
addSpace = add [space]

space, lambda, upperLambda :: Char 
space = ' '
lambda = 'λ'
upperLambda = 'Λ'