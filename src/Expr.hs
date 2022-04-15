module Expr where

import Data.Monoid
import PrettyPrint

data Expr name typ
  = Var name
  | Abstraction
      { param_abs :: name,
        param_type_abs :: Type typ,
        body_abs :: Expr name typ
      }
  | Application
      { function_app :: Expr name typ,
        argument_app :: Expr name typ
      }
  | TypeAbstraction
      { param :: typ,
        body :: Expr name typ
      }
  | TypeApplication
      { function :: Expr name typ,
        argument :: Type typ
      }
  deriving (Eq, Show)

data Type name
  = TypeVar name
  | TypeArrow {param_type :: Type name, body_type :: Type name}
  | TypeForall {param_forall :: name, return_type :: Type name}
  deriving (Eq, Show)

instance (PrettyPrint n, PrettyPrint t) => PrettyPrint (Expr n t) where
  prettyPrint = prettyPrint . ppExpr empty

instance PrettyPrint n => PrettyPrint (Type n) where
  prettyPrint = prettyPrint . ppType empty True

ppExpr :: (PrettyPrint n, PrettyPrint t) => Print String -> Expr n t -> Print String
ppExpr pnt expr = case expr of 
           (Var n) -> prettyPrint n `add` pnt
           (Application e1 e2) -> ppApplication pnt e1 e2
           (Abstraction n t body) -> ppAbstaction pnt n t body
           (TypeAbstraction t body) -> ppTypeAbstraction pnt t body
           (TypeApplication e ty) -> ppTypeApplication pnt e ty

ppApplication :: (PrettyPrint n, PrettyPrint t) => Print String -> Expr n t -> Expr n t -> Print String
ppApplication pnt e1@Abstraction {} e2@Abstraction {} = parens (ppExpr pnt e1) pnt `mappend` addSpace (parens (ppExpr pnt e2) pnt)
ppApplication pnt e1 e2@Application {} = ppExpr pnt e1 `mappend` addSpace (parens (ppExpr pnt e2) pnt)
ppApplication pnt e1 e2@Abstraction {} = ppExpr pnt e1 `mappend` addSpace (parens (ppExpr pnt e2) pnt)
ppApplication pnt e1@Abstraction {} e2 = parens (ppExpr pnt e1) pnt `mappend` addSpace (ppExpr pnt e2)
ppApplication pnt e1 e2 = ppExpr pnt e1 `mappend` addSpace (ppExpr pnt e2)

ppTypeApplication :: (PrettyPrint n, PrettyPrint t) => Print String -> Expr n t -> Type t -> Print String
ppTypeApplication pnt expr ty = pprtexpr `mappend` addSpace (between pprtty' "[" "]" empty)
  where
    pprtexpr = ppExpr pnt expr
    pprtty' = add (prettyPrint ty) empty

ppAbstaction :: (PrettyPrint n, PrettyPrint t) => Print String -> n -> Type t -> Expr n t -> Print String
ppAbstaction pnt name ty body = between vars' lambda' ". " (ppExpr pnt body')
  where
    (vars, body') = uncurryAbs name ty body
    vars' = intercalate (map (uncurry ppArg) vars) " " empty
    lambda' = [lambda, space]
    ppArg n t = prettyPrint n ++ (':' : pprtArg t)
    pprtArg t =
      case t of
        t@(TypeVar _) -> prettyPrint t
        t@(TypeArrow _ _) -> prettyPrint $ parens (ppType empty False t) empty
        _ -> undefined -- TODO: error

ppType :: PrettyPrint n => Print String -> Bool -> Type n -> Print String
ppType pnt space (TypeVar n) = prettyPrint n `add` pnt
ppType pnt space (TypeArrow a b) = ppTypeArrow pnt space a b
ppType pnt _ (TypeForall n t) = ppTypeForall pnt n t

ppTypeArrow :: PrettyPrint n => Print String -> Bool -> Type n -> Type n -> Print String
ppTypeArrow pnt space a b =
  case (a, b) of
    (a@(TypeVar _), b) -> ppTypeArrow' space (ppType pnt space a) (ppType pnt space b)
    (TypeArrow a1 a2, b) -> ppTypeArrow' space (parens (ppTypeArrow pnt space a1 a2) empty) (ppType pnt space b)
    _ -> undefined -- TODO: error

ppTypeArrow' :: Bool -> Print String -> Print String -> Print String
ppTypeArrow' space a b = a <> arrow <> b
  where
    arrow
      | space = " -> " `add` empty
      | otherwise = "->" `add` empty

ppTypeForall :: PrettyPrint n => Print String -> n -> Type n -> Print String
ppTypeForall pnt n t = prefix <> prettyPrint t `add` pnt
  where
    prefix = between (prettyPrint n `add` empty) "forall " ". " empty

ppTypeAbstraction :: (PrettyPrint n, PrettyPrint t) => Print String -> t -> Expr n t -> Print String
ppTypeAbstraction pnt ty body = between vars' lambda' ". " (ppExpr pnt body')
  where
    (vars, body') = uncurryTypeAbs ty body
    vars' = intercalate (map prettyPrint vars) " " empty
    lambda' = [upperLambda, space]

uncurryAbs :: n -> Type t -> Expr n t -> ([(n, Type t)], Expr n t)
uncurryAbs name ty = uncurry' [(name, ty)]
  where
    uncurry' ns (Abstraction n' t' body') = uncurry' ((n', t') : ns) body'
    uncurry' ns body' = (reverse ns, body')

uncurryTypeAbs :: t -> Expr n t -> ([t], Expr n t)
uncurryTypeAbs ty = uncurry' [ty]
  where
    uncurry' ts (TypeAbstraction t' body') = uncurry' (t' : ts) body'
    uncurry' ts body' = (reverse ts, body')