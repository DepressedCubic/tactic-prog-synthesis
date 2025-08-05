module Synthesis where

import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment
import System.IO
import Data.Maybe

import AST
import Parser
import Environment

-- Tactics

type Tactic = TypeEnvironment -> Type -> Maybe Expression
-- The tactic could also tell you how useful it thinks it is.

-- We could have two versions of some tactics.

{-
Given _ : T, gets all variables of type T.
[Variable synthesis]
-}
{-
var :: Tactic
var env t = 
  map (to_var . fst) $ filter (\x -> snd x == t) env
  where
    to_var s = (Var s)
-}

param_var :: String -> Tactic
param_var name env t =
  case (lookup name env) of
    Nothing -> Nothing
    Just t' ->
      case (unify [] t t') of
        Just sub -> Just (Var name)
        Nothing -> Nothing


-- [Boolean synthesis]
prim_bool :: Bool -> Tactic
prim_bool b _ t =
  case t of
    PrimType BOOL -> Just (Boolean b)
    _ -> Nothing

-- [Integer synthesis]
prim_int :: Integer -> Tactic
prim_int n _ t =
  case t of
    PrimType INT -> Just (Num n)
    _ -> Nothing

{-
Given x : T and _ : T -> U, synthesizes
\(x : T) -> (_ : U).
[Lambda synthesis]
-}
intro :: String -> Tactic
intro x env t =
  case t of
    Func r s -> Just (Lambda (TypeAnnotation x r) (Hole s))
    _ -> Nothing

{-
Given type T and _ : U, synthesizes
(_ : T -> U) (_ : T).
[Application synthesis]
-}
apply :: Type -> Tactic
apply t _ u = Just (App (Hole (Func t u)) (Hole t))

-- Auxiliary functions for function application synthesis
split_function_type :: Type -> [Type]
split_function_type t =
  case t of
    Func t1 ts -> t1 : (split_function_type ts)
    _ -> [t]



{-
Given f : T1 -> ... -> Tn -> U and _ : U, synthesizes
f (_ : T1) ... (_ : Tn)
[Function application synthesis]
-}
f_apply :: String -> Tactic
f_apply f env u =
  let
    subs sub (Hole t) = Hole (apply_sub sub t)
    subs sub x = x
  in
    case (lookup f env) of
      Nothing -> Nothing
      Just t ->
        let
          types = split_function_type t
          rev_types = reverse types
          u' = head rev_types
          ts = tail rev_types
          exps = (map (\t -> Hole t) ts) ++ [(Var f)]
        in
          case (unify [] u u') of
            Just sub -> 
              Just (app_builder $ map (subs sub) (exps))
            _ -> Nothing


{-
Given _ : T, synthesizes
if (_ : Bool) then (_ : T) else ( : T).
[Conditional synthesis]
-}
cases :: Tactic
cases _ t = Just (Ifte (Hole (PrimType BOOL)) (Hole t) (Hole t))

-- "Generalized tactic"

{-
complete :: TypeEnvironment -> [Tactic] -> Type -> [Expression]
complete env tactics t =
  mapMaybe (\tactic -> tactic env t) tactics
-}

-- Auxiliary functions for dealing with expressions

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


{-
available_id exp:
Determines the first available ID that is larger than
any ID in exp.
-}
available_id :: Expression -> Int
available_id exp =
  case exp of
    IDHole id t -> id + 1
    _ -> max_helper $ map available_id $ subexp exp
  where
    max_helper [] = 0
    max_helper xs = maximum xs

{-
tp_fill filler id (tp, next_id):
Assuming that 'filler' has been ID'd correctly, we fill the TopLevel
'tp' with it in the hole 'id', if it exists, assuming that 'next_id'
is the known next available ID. Alongside it, we return an updated next_id'.
-}
tp_fill :: Expression -> Int -> (TopLevel, Int) -> (TopLevel, Int)
tp_fill filler id (tp, next_id) =
  case tp of
    (Let ann exp) -> (Let ann (exp_fill filler id exp), next_id')
    (LetRec ann exp) -> (LetRec ann (exp_fill filler id exp), next_id')
  where
    next_id' = max (available_id filler) next_id
    exp_fill filler id exp =
      case exp of
        IDHole id' t -> if (id == id') 
          then filler
          else IDHole id' t
        _ -> exp_map (exp_fill filler id) exp

code_fill :: Expression -> Int -> ([TopLevel], Int) -> ([TopLevel], Int)
code_fill filler id (code, next_id) =
  case code of
    [] -> ([], next_id)
    tp : tps ->
      let
        (tp', next_id') = tp_fill filler id (tp, next_id)
        (tps', next_id'') = code_fill filler id (tps, next_id')
      in
        (tp' : tps', next_id'')



{-
find_type_env type_env id tp:
Given a type environment type_env, in 'tp' looks for the type of the hole 'id'
(if it exists) and alongside it, returns the 'local' type environment at 
the hole.
-}
find_type_env :: TypeEnvironment -> Int -> TopLevel -> Maybe (Type, TypeEnvironment)
find_type_env type_env id tp =
  case tp of
    (Let _ exp) -> fnd type_env id exp
    (LetRec _ exp) -> fnd type_env id exp
  where
    fnd type_env id exp =
      case exp of
        IDHole id' t -> if (id' == id) 
          then Just (t, type_env)
          else Nothing
        Lambda (TypeAnnotation var t) e1 -> 
          fnd ((var, t) : type_env) id e1
        _ -> listToMaybe $ catMaybes $ map (fnd type_env id) $ subexp exp

global_find_type_env :: TypeEnvironment -> Int -> [TopLevel] -> Maybe (Type, TypeEnvironment)
global_find_type_env type_env id code =
  listToMaybe $ catMaybes $ map (find_type_env type_env id) code

{-
get_type_env type_env code:
Given a list of top-levels 'code', gets the top-level type environment, with
type_env as the initial types.
Note: it doesn't type check -- it only returns what the claimed types
are.
-}
get_type_env :: [TopLevel] -> TypeEnvironment -> TypeEnvironment
get_type_env code type_env =
  case code of
    [] -> type_env
    tp : tps ->
      case tp of
        Let (TypeAnnotation var t) _ -> (var, t) : (get_type_env tps type_env)
        LetRec (TypeAnnotation var t) _ -> 
          (var, t) : (get_type_env tps type_env)

{-
add_ids (exp, next_id):
Given an expression exp, it adds IDs to the holes without IDs; starting
at ID next_id. Then, (exp', next_id') is returned -- where exp' and next_id'
are the updated expressions and next available, respectively.
-}
add_ids :: (Expression, Int) -> (Expression, Int)
add_ids (exp, next_id) =
  case exp of
    Hole t -> (IDHole next_id t, next_id + 1)
    App e1 e2 ->
      let
        (e1', next_id') = add_ids (e1, next_id)
        (e2', next_id'') = add_ids (e2, next_id')
      in
        (App e1' e2', next_id'')
    Ifte cond e1 e2 ->
      let
        (cond', next_id') = add_ids (cond, next_id)
        (e1', next_id'') = add_ids (e1, next_id')
        (e2', next_id''') = add_ids (e2, next_id'')
      in
        (Ifte cond' e1' e2', next_id''')
    Lambda ann e ->
      let
        (e', next_id') = add_ids (e, next_id)
      in
        (Lambda ann e', next_id')
    Pair e1 e2 ->
      let
        (e1', next_id') = add_ids (e1, next_id)
        (e2', next_id'') = add_ids (e2, next_id')
      in
        (Pair e1' e2', next_id'')
    _ -> (exp, next_id)

