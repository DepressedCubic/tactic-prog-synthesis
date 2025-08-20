module Complete where

import AST
import Environment
import Synthesis2
import Data.List
import Data.Maybe


variable_gen :: TypeEnvironment -> String
variable_gen env =
  head $ filter (\v -> (lookup v env) == Nothing) $ map (\n -> "x" ++ (show n)) [1..]

-- Given a parametrized tactic and a list of parameters, makes a poly-tactic


-- NEW
-- ignores all dead-end tactics, and sorts by usefulness.
complete :: HoleInfo -> [Tactic] -> [(Tactic, Expression)]
complete info tactics =
  let
    valid = filter (\tc -> isJust $ result tc) tactics
    triplets = map (\tc -> (tc, fromJust $ result tc)) valid
    sorted = reverse $ sortOn (\(_, (_, x)) -> x) triplets
  in
    map (\(tc, (exp, _)) -> (tc, exp)) sorted
  where
    result tc = fst $ tc info initial_state

tactic_list :: TypeEnvironment -> [Tactic]
tactic_list env =
  let
    vars = map (param_var . fst) env
    bools = map prim_bool [True, False]
    ints = map prim_int [0..10]
    intros = map intro [variable_gen env]
    applys = map (f_apply . fst) env
  in
    vars ++ bools ++ ints ++ intros ++ applys ++ [cases]

  
show_complete :: [(Tactic, Expression)] -> String
show_complete pairs =
  let
    exps = map snd pairs
    indexed = zip [1..] exps
  in
    concatMap (\(n, exp) -> (show n) ++ ". " ++ (show exp) ++ "\n") indexed