module Expr where

import Data.Monoid

data Expr name typ
  = Var name
  | Abstraction { param_abs :: name, param_type_abs :: Type typ, body_abs :: Expr name typ }
  | Application { function_app :: Expr name typ, argument_app :: Expr name typ }
  | TypeAbstraction { param :: typ, body :: Expr name typ }
  | TypeApplication { function :: Expr name typ, argument :: Type typ } 
  deriving (Eq, Show)

data Type name
  = TypeVar name
  | TypeArrow { param_type :: Type name, body_type :: Type name}
  | TypeForall { param_forall :: name, return_type :: Type name }
  deriving (Eq, Show)

{-
  TODO: add instances for pretty print
-}