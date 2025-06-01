module Interpreter where

import Data.Maybe
import AST
import Environment

-- TYPING

-- is_instantiated t holds whenever t has no type placeholders.
is_instantiated :: Type -> Bool
is_instantiated T = False
is_instantiated (PrimType t) = True
is_instantiated (List t) = is_instantiated t
is_instantiated (Func t1 t2) = (is_instantiated t1) && (is_instantiated t2)
is_instantiated (Prod t1 t2) = (is_instantiated t1) && (is_instantiated t2)
is_instantiated Any = False

{-
instantiate_type t1 t2 will replace all placeholder types T
in t1 with t2.
-}
instantiate_type :: Type -> Type -> Type
instantiate_type Any t2 = Any
instantiate_type T t2 = t2
instantiate_type (PrimType t) _ = (PrimType t)
instantiate_type (List t) t2 = (List (instantiate_type t t2))
instantiate_type (Func t0 t1) t2 =
  (Func (instantiate_type t0 t2) (instantiate_type t1 t2))
instantiate_type (Prod t0 t1) t2 =
  (Prod (instantiate_type t0 t2) (instantiate_type t1 t2))

data TypeChoice = Instantiated | Subs Type | None
  deriving (Eq, Show)

match_pairs :: (Type, Type) -> (Type, Type) -> TypeChoice
match_pairs (t0, t1) (u0, u1) =
  let
    c0 = match_types t0 u0
    c1 = match_types t1 u1
  in
    case (c0, c1) of
      (None, _) -> None
      (_, None) -> None
      (Instantiated, _) -> c1
      (_, Instantiated) -> c0
      (Subs m0, Subs m1) ->
        if (m0 == m1)
          then (Subs m0)
          else None

{-
match_types t t':
If the two types don't match or t' isn't instantiated, returns None
(thus, it is expected that the second argument is always instantiated).
If they match and t was already instantiated, returns Instantiated.
Otherwise (i.e. they match but t wasn't instantiated), returns
(Subs t1), where t1 is the type to substitute the placeholder type in t
to make instantiate it and make it match t'.
-}
match_types :: Type -> Type -> TypeChoice
match_types _ T = None
match_types Any _ = Instantiated
match_types (Prod t0 t1) (Prod u0 u1) =
  match_pairs (t0, t1) (u0, u1)
match_types (Func t0 t1) (Func u0 u1) =
  match_pairs (t0, t1) (u0, u1)
match_types (Func t0 t1) _ = None
match_types (List t) (List u) =
  match_types t u
match_types (List t) _ = None
match_types (PrimType t) (PrimType u) =
  if (t == u) then Instantiated else None
match_types (PrimType t) _ = None
match_types T t0 = (Subs t0)

{-
get_type exp env:
If exp is well-typed and of type U in type environment env,
returns Just U.
Otherwise, returns Nothing.
-}
get_type :: Expression -> TypeEnvironment -> Maybe Type
get_type (App e1 e2) env = do
  t1 <- get_type e1 env
  t2 <- get_type e2 env
  let
    b1 = is_instantiated t1
    b2 = is_instantiated t2
  case (b1, b2) of
    (True, True) ->
      case t1 of
        Func r s -> if (r == t2) then return (s) else Nothing
        _ -> Nothing
    (False, True) ->
      case t1 of
        Func r s ->
          case (match_types r t2) of
            Instantiated -> return (s)
            Subs t -> return (instantiate_type s t)
            None -> Nothing
        _ -> Nothing
    (True, False) ->
      case t1 of
        Func r s ->
          case (match_types t2 r) of
            None -> Nothing
            _ -> return (s)
        _ -> Nothing
    _ -> Nothing 
{-
This last case implies that for f x to be considered well-typed here,
either f or x must have fully instantiated type.
-}
get_type (Var s) env = lookup s env
get_type (Ifte c e1 e2) env = do
  t <- get_type c env
  t1 <- get_type e1 env
  t2 <- get_type e2 env
  if (t == PrimType BOOL)
    then
      let
        b1 = is_instantiated t1
        b2 = is_instantiated t2
      in
        case (b1, b2) of
          (True, True) -> if (t1 == t2) then return (t1) else Nothing
          (False, True) ->
            case (match_types t1 t2) of
              None -> Nothing
              _ -> return (t2)
          (True, False) ->
            case (match_types t2 t1) of
              None -> Nothing
              _ -> return (t1)
          _ -> Nothing
{-
This last case implies that for 'if (c) then (e1) else (e2)' to be
considered well-typed here, either e1 or e2 must have fully instantiated type.
-}
    else Nothing
get_type (Lambda (TypeAnnotation name t1) exp) env = do
  t2 <- get_type exp ((name, t1) : env)
  return (Func t1 t2)
get_type (Num n) env = Just (PrimType INT)
get_type (Boolean b) env = Just (PrimType BOOL)
get_type (Pair a b) env = do
  t1 <- get_type a env
  t2 <- get_type b env
  return (Prod t1 t2)

{-
correct_type env tp:
Holds exactly when the top-level 'tp' is well-typed in type environment 'env'.
-}
correct_type :: TypeEnvironment -> TopLevel -> Bool
correct_type env (Let (TypeAnnotation _ t) exp) =
  case (get_type exp env) of
    Just u -> (t == u)
    Nothing -> False
correct_type env (LetRec (TypeAnnotation name t) exp) =
  case (get_type exp ((name, t) : env)) of
    Just u -> (t == u)
    Nothing -> False

{-
update_types env tp:
Returns an updated version of the type environment 'env' in light
of the top-level function declaration 'tp'. Doesn't perform any type
checking, so it must only be run if correct_type has already been run.
-}
update_types :: TypeEnvironment -> TopLevel -> TypeEnvironment
update_types env tp =
  case tp of
    Let (TypeAnnotation name t) _ -> (name, t) : env
    LetRec (TypeAnnotation name t) _ -> (name, t) : env

-- values can be Haskell ints, bools, lists, or functions!
{-
eval exp env tp:
Returns the interpreted Value of 'exp', in light of the current
value environment 'env', and the possible top-level declaration
that 'exp' belongs to (this is important when evaluating
recursive functions).
-}
eval :: Expression -> ValueEnvironment -> TopLevel -> Value
eval (App e1 e2) env tp =
  let
    v1 = eval e1 env tp
    v2 = eval e2 env tp
  in
    case v1 of
      Recurse -> case tp of
        LetRec _ (Lambda (TypeAnnotation name t) low_exp) ->
          eval low_exp ((name, v2) : env) tp
      Function f -> f v2 
-- This implies that you can't apply a function to a recursive call.
eval (Var s) env tp = fromJust (lookup s env)
eval (Ifte cond e1 e2) env tp =
  let
    cond_val = eval cond env tp
  in
    case cond_val of
      Bool True -> eval e1 env tp
      Bool False -> eval e2 env tp
eval (Lambda (TypeAnnotation name t) e) env tp =
  Function (\x -> eval e ((name, x) : env) tp)
eval (Num n) env tp = Int n
eval (Boolean b) env tp = Bool b
eval (Pair a b) env tp = PairValue (eval a env tp, eval b env tp)

{-
update_environment env tp:
Returns the updated version of the value environment 'env', after
having interpreted top-level 'tp'.
-}
update_environment :: ValueEnvironment -> TopLevel -> ValueEnvironment
update_environment env tp =
  case tp of
    Let (TypeAnnotation x t) e -> (x, eval e env tp) : env
    LetRec (TypeAnnotation x t) e -> (x, eval e ((x, Recurse) : env) tp) : env