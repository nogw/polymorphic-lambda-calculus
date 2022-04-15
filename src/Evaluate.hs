module Evaluate where

import Expr
import Data.List
import Data.Maybe 
import PrettyPrint

-- eval :: (Ord n, Eq n, PrettyPrint n) => Expr n n -> Either String (Expr n n)
-- eval expr = case expr of 
--   Var n -> Right $ Var n
--   Abstraction n ty ex -> Right $ Abstraction n ty ex
--   TypeAbstraction n ex -> Right $ TypeAbstraction n ex
--   Application ex ex' -> do 
--     f <- eval ex 
--     res <- runApplicationlication f <*> eval ex 
--     eval res 
--   TypeApplication ex ty -> do 
--     f <- eval n 
--     res <- runTypeApplication f <*> pure ex 
--     eval res
--   _ -> undefined

-- runApplication (Abstraction { param = t, body = f }) = good f
-- runApplication (Var p) = runApplicationPrim p
-- runApplication _ = err "unexpected non-abstraction used in application"
