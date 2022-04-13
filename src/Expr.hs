module Expr where

import Data.Monoid

import PrettyPrintUtils

data Expr name typ
  = Var name
  | Abstraction { param :: name, param_type :: Type typ, body :: Expr name typ }
  | Application { function :: Expr name typ, argument :: Expr name typ }
  | TypeAbstraction { param :: typ, body :: Expr name typ }
  | TypeApplication { function :: Expr name typ, argument :: Type typ } 
  deriving (Eq, Show)

data Type name
  = TypeVar name
  | TypeArrow { param_type :: Type name, body_type :: Type name}
  | TypeForall { param :: name, return_type :: Type name }
  deriving (Eq, Show)