module Interpreter where

import Data.Maybe
import AST

type TypeEnvironment = [(String, Type)]

type ValueEnvironment = [(String, Value)]

nil_val :: Value
nil_val = ListValue []

cons_val :: Value
cons_val =
  Function (\v -> Function (\(ListValue l) -> (ListValue (v:l))))

head_val :: Value
head_val =
  Function (\(ListValue l) -> (head l))

tail_val :: Value
tail_val =
  Function (\(ListValue l) -> ListValue (tail l))

isnil_val :: Value
isnil_val =
  Function (\(ListValue l) -> (Bool (null l)))

leq_val :: Value
leq_val =
  Function (\(Int v) -> (Function (\(Int w) -> Bool (v <= w))))

and_val :: Value
and_val =
  Function (\(Bool b1) -> (Function (\(Bool b2) -> Bool (b1 && b2))))

or_val :: Value
or_val =
  Function (\(Bool b1) -> (Function (\(Bool b2) -> Bool (b1 || b2))))

not_val :: Value
not_val =
  Function (\(Bool b) -> (Bool (not b)))

add_val :: Value
add_val =
  Function (\(Int x1) -> (Function (\(Int x2) -> Int (x1 + x2))))

sub_val :: Value
sub_val =
  Function (\(Int x1) -> (Function (\(Int x2) -> Int (x1 - x2))))

mul_val :: Value
mul_val =
  Function (\(Int x1) -> (Function (\(Int x2) -> Int (x1 * x2))))

div_val :: Value
div_val =
  Function (\(Int x1) -> (Function (\(Int x2) -> Int (div x1 x2))))

zero_val :: Value
zero_val =
  Function (\(Int x) -> (Bool (x == 0)))

initial_types :: TypeEnvironment
initial_types =
  [
    ("nil", List T),
    ("cons", Func T (Func (List T) (List T))),
    ("head", Func (List T) T),
    ("tail", Func (List T) (List T)),
    ("isnil", Func (List T) (PrimType BOOL)),
    ("leq", Func (PrimType INT) (Func (PrimType INT) (PrimType BOOL))),
    ("and", Func (PrimType BOOL) (Func (PrimType BOOL) (PrimType BOOL))),
    ("or", Func (PrimType BOOL) (Func (PrimType BOOL) (PrimType BOOL))),
    ("not", Func (PrimType BOOL) (PrimType BOOL)),
    ("add", Func (PrimType INT) (Func (PrimType INT) (PrimType INT))),
    ("sub", Func (PrimType INT) (Func (PrimType INT) (PrimType INT))),
    ("mul", Func (PrimType INT) (Func (PrimType INT) (PrimType INT))),
    ("div", Func (PrimType INT) (Func (PrimType INT) (PrimType INT))),
    ("zero", Func (PrimType INT) (PrimType BOOL))
  ]

initial_environment :: ValueEnvironment
initial_environment =
  [
    ("nil", nil_val),
    ("cons", cons_val),
    ("head", head_val),
    ("tail", tail_val),
    ("isnil", isnil_val),
    ("leq", leq_val),
    ("and", and_val),
    ("or", or_val),
    ("not", not_val),
    ("add", add_val),
    ("sub", sub_val),
    ("mul", mul_val),
    ("div", div_val),
    ("zero", zero_val)
  ]

-- TYPING

-- doesn't contain any Ts.
is_instantiated :: Type -> Bool
is_instantiated T = False
is_instantiated (PrimType t) = True
is_instantiated (List t) = is_instantiated t
is_instantiated (Func t1 t2) = (is_instantiated t1) && (is_instantiated t2)

-- instantiate_type t1 t2 will replace all T's
-- in t1 with t2.
instantiate_type :: Type -> Type -> Type
instantiate_type T t2 = t2
instantiate_type (PrimType t) _ = (PrimType t)
instantiate_type (List t) t2 = (List (instantiate_type t t2))
instantiate_type (Func t0 t1) t2 =
  (Func (instantiate_type t0 t2) (instantiate_type t1 t2))

data TypeChoice = Instantiated | Subs Type | None
  deriving (Eq, Show)

-- given any type t and an instantiated type t',
-- if they match, determines the type to substitute
-- T with to instantiate t; if t was already instantiated,
-- returns Instantiated; if they don't match or t' not instantiated,
-- None.
match_types :: Type -> Type -> TypeChoice
match_types _ T = None
match_types (Func t0 t1) (Func u0 u1) =
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
match_types (Func t0 t1) _ = None
match_types (List t) (List u) =
  match_types t u
match_types (List t) _ = None
match_types (PrimType t) (PrimType u) =
  if (t == u) then Instantiated else None
match_types (PrimType t) _ = None
match_types T t0 = (Subs t0)

-- For some expression, returns either Just K if it's
-- well-typed and of type K, or Nothing.
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
    _ -> Nothing -- i.e. to apply, either func or input must have instantiated type.
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
          _ -> Nothing -- at least one of the options must have inst. type.
    else Nothing
get_type (Lambda (TypeAnnotation name t1) exp) env = do
  t2 <- get_type exp ((name, t1) : env)
  return (Func t1 t2)
get_type (Num n) env = Just (PrimType INT)
get_type (Boolean b) env = Just (PrimType BOOL)

correct_type :: TypeEnvironment -> TopLevel -> Bool
correct_type env (Let (TypeAnnotation _ t) exp) =
  case (get_type exp env) of
    Just u -> (t == u)
    Nothing -> False
correct_type env (LetRec (TypeAnnotation name t) exp) =
  case (get_type exp ((name, t) : env)) of
    Just u -> (t == u)
    Nothing -> False

-- WARNING: Only use if correct_type has been run first!
update_types :: TypeEnvironment -> TopLevel -> TypeEnvironment
update_types env tp =
  case tp of
    Let (TypeAnnotation name t) _ -> (name, t) : env
    LetRec (TypeAnnotation name t) _ -> (name, t) : env

-- values can be Haskell ints, bools, lists, or functions!
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
      Function f -> f v2 -- i.e. for now, a recursive call can't be an arg
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

update_environment :: ValueEnvironment -> TopLevel -> ValueEnvironment
update_environment env tp =
  case tp of
    Let (TypeAnnotation x t) e -> (x, eval e env tp) : env
    LetRec (TypeAnnotation x t) e -> (x, eval e ((x, Recurse) : env) tp) : env