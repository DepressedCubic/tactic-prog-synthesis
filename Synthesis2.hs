module Synthesis2 where

import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment
import System.IO
import Data.Maybe
import Data.List

import AST
import Parser
import Environment

-- state: code, type env, next id, current hole, next type id.

data S = State {
  code :: [TopLevel],
  env :: TypeEnvironment,
  next_id :: Int,
  hole :: Int,
  next_type :: Int
} deriving (Show)

initial_state :: S
initial_state = Synthesis2.State {code = [], env = initial_types, next_id = 0, hole = -1, next_type = 0}

-- tactics

type HoleInfo = (TypeEnvironment, Type)

type Tactic = HoleInfo -> S -> (Maybe (Expression, Int), S)

next_hole :: [TopLevel] -> Int -> Int
next_hole code id =
  let
    ids = concatMap tp_ids code
    remaining_ids = dropWhile (\n -> n /= id) ids
  in
    if ((length ids) <= 1)
      then -1
      else
        case remaining_ids of
          [] -> head ids
          [x] -> head ids
          (x : xs) ->  head xs
  where
    tp_ids (Let _ e) = list_ids e
    tp_ids (LetRec _ e) = list_ids e

-- IMPORTANT: Check carefully which of these add new type variables
-- (so that next_type is updated accordingly).

param_var :: String -> Tactic
param_var name (env, t) s =
  case (lookup name env) of
    Nothing -> (Nothing, s)
    Just t' ->
      let
        (next, t'') = shift_ids (next_type s, t')
      in
        case (unify [] t t'') of
          Just sub -> 
            (Just (Var name, 20), 
            s{code = (global_apply_sub sub $ code s), 
            hole = next_hole (code s) (hole s),
            next_type = next})
          Nothing -> (Nothing, s)

-- [Boolean synthesis]
prim_bool :: Bool -> Tactic
prim_bool b (_, t) s =
  case t of
    PrimType BOOL -> (Just (Boolean b, 5), s{hole = next_hole (code s) (hole s)})
    _ -> (Nothing, s)

-- [Integer synthesis]
prim_int :: Integer -> Tactic
prim_int n (_, t) s =
  case t of
    PrimType INT -> (Just (Num n, 7), s{hole = next_hole (code s) (hole s)})
    _ -> (Nothing, s)

{-
Given x : T and _ : T -> U, synthesizes
\(x : T) -> (_ : U).
[Lambda synthesis]
-}
intro :: String -> Tactic
intro x (env, t) s =
  case t of
    Func u v ->
      let
        id = next_id s
      in
        (Just (Lambda (TypeAnnotation x u) (IDHole id v), 25), 
        s{next_id = id + 1, hole = id})
    _ -> (Nothing, s)

{-
Given type T and _ : U, synthesizes
(_ : T -> U) (_ : T).
[Application synthesis]
-}
-- another version of apply that takes no type and makes a type variable;
-- but take care that the type variable is updated everywhere.
{-
Given _ : U, synthesizes
(_ : T(0) -> U) (_ : T(0))
[Generalized apply]
-}
apply :: Type -> Tactic
apply t (_, u) s = 
  let
    id = next_id s
  in
    (Just (App (IDHole (id) (Func t u)) (IDHole (id + 1) t), 8),
     s{next_id = id + 2, hole = id})

-- Auxiliary functions for function application synthesis
split_function_type :: Type -> [Type]
split_function_type t =
  case t of
    NoShift t' -> split_function_type t'
    Func t1 ts -> t1 : (split_function_type ts)
    _ -> [t]

shift_ids :: (Int, Type) -> (Int, Type)
shift_ids (next, t) =
  let
    next' = next + (length $ ids t)
    t' = shift next t
  in
    case t of
      NoShift _ -> (next, t)
      _ -> (next', t')
  where
    ids (Func t1 t2) = union (ids t1) (ids t2)
    ids (Prod t1 t2) = union (ids t1) (ids t2)
    ids (List t) = ids t
    ids (TypeVar n) = [n]
    ids _ = []
    shift k (Func t1 t2) = Func (shift k t1) (shift k t2)
    shift k (Prod t1 t2) = Prod (shift k t1) (shift k t2)
    shift k (List t) = List (shift k t)
    shift k (TypeVar n) = TypeVar (k + n)
    shift k t = t


{-
Given f : T1 -> ... -> Tn -> U and _ : U, synthesizes
f (_ : T1) ... (_ : Tn)
[Function application synthesis]
-}
-- Thus far, the only used tactic that updates the type ID counter.
f_apply :: String -> Tactic
f_apply f (env, u) s =
  let
    subs sub (IDHole n t) = IDHole n (apply_sub sub t)
    subs sub x = x
  in
    case (lookup f env) of
      Nothing -> (Nothing, s)
      Just t ->
        let
          (next, t') = shift_ids (next_type s, t)
          types = split_function_type t'
          rev_types = reverse types
          u' = head rev_types
          id = next_id s
          ts = map (\(n, t) -> IDHole (id + n) t) (zip [0..] $ tail rev_types)
          exps = ts ++ [(Var f)]
        in
          case (unify [] u u') of
            Just sub -> 
              (Just (app_builder $ map (subs sub) (exps), 15),
               s{code = (global_apply_sub sub $ code s),
               next_id = id + (length ts), 
               hole = id + (length ts) - 1,
                 next_type = next})
            _ -> (Nothing, s)

-- BUT WAIT!! We do not need to shift the IDs of type variables that already occur.


{-
Given _ : T, synthesizes
if (_ : Bool) then (_ : T) else ( : T).
[Conditional synthesis]
-}
cases :: Tactic
cases (_, t) s =
  let
    id = next_id s
  in
    (Just (Ifte (IDHole id (PrimType BOOL)) (IDHole (id + 1) t) (IDHole (id + 2) t), 17), s{next_id = id + 3, hole = id})

-- Named tactics

param_varN :: String -> Named Tactic
param_varN name = Named ("var " ++ name) (param_var name)

prim_boolN :: Bool -> Named Tactic
prim_boolN b = Named ("bool " ++ (show b)) (prim_bool b)

prim_intN :: Integer -> Named Tactic
prim_intN n = Named ("int " ++ (show n)) (prim_int n)

introN :: String -> Named Tactic
introN x = Named ("intro " ++ x) (intro x)

f_applyN :: String -> Named Tactic
f_applyN f = Named ("apply " ++ f) (f_apply f)

casesN :: Named Tactic
casesN = Named "cases" cases


-- Auxiliaries

{-
exp_map f exp:
Applies f to all the 'children' of exp that are expressions.
-}
exp_map :: (Expression -> Expression) -> Expression -> Expression
exp_map f exp =
  case exp of
    App e1 e2 -> App (f e1) (f e2)
    Var s -> Var s
    Ifte cond e1 e2 -> Ifte (f cond) (f e1) (f e2)
    Lambda t e -> Lambda t (f e)
    Pair e1 e2 -> Pair (f e1) (f e2)
    _ -> exp

{-
subexp exp:
Returns a list of all the subexpressions in exp.
-}
subexp :: Expression -> [Expression]
subexp exp =
  case exp of
    App e1 e2 -> [e1, e2]
    Ifte cond e1 e2 -> [cond, e1, e2]
    Lambda _ e -> [e]
    Pair e1 e2 -> [e1, e2]
    _ -> []

fill :: Expression -> Int -> TopLevel -> TopLevel
fill filler id tp =
  case tp of
    (Let ann exp) -> Let ann (exp_fill filler id exp)
    (LetRec ann exp) -> LetRec ann (exp_fill filler id exp)
  where
    exp_fill filler id exp =
      case exp of
        IDHole id' t -> if (id == id') then filler else IDHole id' t
        _ -> exp_map (exp_fill filler id) exp

code_fill :: Expression -> Int -> [TopLevel] -> [TopLevel]
code_fill filler id code = map (fill filler id) code

{-
find_type_env type_env id tp:
Given a type environment type_env, in 'tp' looks for the type of the hole 'id'
(if it exists) and alongside it, returns the 'local' type environment at 
the hole.
-}
find_type_env :: TypeEnvironment -> Int -> TopLevel -> Maybe HoleInfo
find_type_env type_env id tp =
  case tp of
    (Let _ exp) -> fnd type_env id exp
    (LetRec _ exp) -> fnd type_env id exp
  where
    fnd type_env id exp =
      case exp of
        IDHole id' t -> if (id' == id) 
          then Just (type_env, t)
          else Nothing
        Lambda (TypeAnnotation var t) e1 -> 
          fnd ((var, NoShift t) : type_env) id e1
        _ -> listToMaybe $ catMaybes $ map (fnd type_env id) $ subexp exp

global_find_type_env :: TypeEnvironment -> Int -> [TopLevel] -> Maybe HoleInfo
global_find_type_env type_env id code =
  listToMaybe $ catMaybes $ map (find_type_env type_env id) code