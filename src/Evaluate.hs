module Evaluate where

import Data.Either
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Expr hiding (PrettyPrint)
import PrettyPrint

type Context n = Map.Map n (Expr n n)

type TypeContext n = Map.Map n (Type n)

type ValueContext n = Map.Map n (Value n)

data Value n
  = VClosure {context :: ValueContext n, param_cls :: n, body_cls :: Expr n n}
  | VForall {context :: ValueContext n, body_forall :: Expr n n}
  | VNative (Value n -> Value n)
  deriving (Show)

instance Show (a -> b) where -- temporary / debug
  show a = "VNative ;)"

eval ::
  (Ord n, Eq n) =>
  ValueContext n ->
  Expr n n ->
  Value n -- todo: either
eval context expr = case expr of
  Var n -> fromJust $ Map.lookup n context
  Abstraction n ty ex -> VClosure {context = context, param_cls = n, body_cls = ex}
  Application function argument ->
    let argument' = eval context argument
     in case eval context function of
          VClosure {context = context, param_cls = param, body_cls = body} ->
            eval (Map.insert param argument' context) body
          VNative f -> f argument'
          VForall {} -> error "TODO: error message"
  TypeAbstraction _ ex -> VForall {context = context, body_forall = ex}
  TypeApplication function ty ->
    case eval context function of
      VForall {context = context, body_forall = body} -> eval context body
      VClosure {} -> error "TODO: error message"
      VNative _ -> error "TODO: error message"