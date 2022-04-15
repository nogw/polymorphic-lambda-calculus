module TypeCheck where

import Data.Map as M
import Expr
import PrettyPrint

type UniqueSupply n = [n]

type Context k w = Map k w

typecheck :: (Ord n, Eq n, PrettyPrint n) => UniqueSupply n -> Context n (Type n) -> Expr n n -> Either String (Type n)
typecheck uniqs context expr =
  case expr of
    Var n -> typecheckVar uniqs context n
    Abstraction n ty ex -> typecheckAbstraction uniqs context n ty ex
    Application ex ex' -> typecheckApplication uniqs context ex ex'
    TypeAbstraction n ex -> typecheckTypeAbstraction uniqs context n ex
    TypeApplication ex ty -> typecheckTypeApplication uniqs context ex ty

typecheckVar :: (Ord n, Eq n, PrettyPrint n) => UniqueSupply n -> Context n (Type n) -> n -> Either String (Type n)
typecheckVar uniqs context variable = maybe (TypeVar <$> unique uniqs) return (M.lookup variable context)

typecheckAbstraction :: (Ord n, Eq n, PrettyPrint n) => UniqueSupply n -> Context n (Type n) -> n -> Type n -> Expr n n -> Either String (Type n)
typecheckAbstraction uniqs context name ty body = TypeArrow ty <$> typecheck uniqs context' body
  where
    context' = insert name ty context

typecheckApplication :: (Ord n, Eq n, PrettyPrint n) => UniqueSupply n -> Context n (Type n) -> Expr n n -> Expr n n -> Either String (Type n)
typecheckApplication uniqs context e1 e2 = do
  t1 <- typecheck uniqs context e1
  t2 <- typecheck uniqs context e2
  (t3, t4) <- either genMismatchVar return (arrow t1)
  if t3 == t2
    then return t4
    else Left $ tyMismatchMsg (TypeArrow t2 t4) (TypeArrow t1 t4)
  where
    genMismatchVar expected = unique uniqs >>= Left . tyMismatchMsg expected
    arrow (TypeArrow t1 t2) = return (t1, t2)
    arrow t = Left t

typecheckTypeAbstraction :: (Ord n, Eq n, PrettyPrint n) => UniqueSupply n -> Context n (Type n) -> n -> Expr n n -> Either String (Type n)
typecheckTypeAbstraction uniqs context typ body = TypeForall typ <$> typecheck uniqs context' body
  where
    context' = M.insert typ (TypeVar typ) context

typecheckTypeApplication :: (Ord n, Eq n, PrettyPrint n) => UniqueSupply n -> Context n (Type n) -> Expr n n -> Type n -> Either String (Type n)
typecheckTypeApplication uniqs context (TypeAbstraction t expr) typ = typecheck uniqs context expr'
  where
    expr' = subst t typ expr
typecheckTypeApplication uniqs context expr typ = typecheck uniqs context expr

unique :: UniqueSupply t -> Either String t
unique (u : _) = Right u
unique _ = Left "i didn't think of a nice message, but that failed"

subst :: Eq n => n -> Type n -> Expr n n -> Expr n n
subst name typ expr =
  case expr of
    Application e1 e2 ->
      Application
        { function_app = subst name typ e1,
          argument_app = subst name typ e2
        }
    Abstraction n ty ex ->
      Abstraction
        { param_abs = n,
          param_type_abs = substType n typ ty,
          body_abs = subst name typ ex
        }
    TypeAbstraction ty ex ->
      TypeAbstraction
        { param = ty,
          body = subst name typ ex
        }
    TypeApplication ex ty ->
      TypeApplication
        { function = subst name typ ex,
          argument = substType name typ ty
        }
    expr -> expr

substType :: Eq n => n -> Type n -> Type n -> Type n
substType name typ (TypeArrow t1 t2) =
  TypeArrow
    { param_type = substType name typ t1,
      body_type = substType name typ t2
    }
substType name typ typ1@(TypeVar var)
  | name == var = typ
  | otherwise = typ1
substType name t1 t2@(TypeForall var t2')
  | name == var = t2
  | otherwise =
    TypeForall
      { param_forall = var,
        return_type = substType name t2 t2'
      }

tyMismatchMsg :: (PrettyPrint typ, PrettyPrint typ1) => typ -> typ1 -> String
tyMismatchMsg expected actual =
  "expected type " <> prettyPrint expected
    <> " found "
    <> prettyPrint actual