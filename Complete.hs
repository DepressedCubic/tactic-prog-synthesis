module Complete where

import AST
import Environment
import Synthesis2
import Data.List
import Data.Maybe


variable_gen :: TypeEnvironment -> String
variable_gen env =
  head $ filter (\v -> (lookup v env) == Nothing) $ map (\n -> "x" ++ (show n)) [1..]


-- NEW
-- ignores all dead-end tactics, and sorts by usefulness.
complete :: HoleInfo -> [Named Tactic] -> [(Named Tactic, Expression)]
complete info tactics =
  let
    valid = filter (\tc -> isJust $ result tc) tactics
    triplets = map (\tc -> (tc, fromJust $ result tc)) valid
    sorted = reverse $ sortOn (\(_, (_, x)) -> x) triplets
  in
    map (\(tc, (exp, _)) -> (tc, exp)) sorted
  where
    result tc = fst $ (run tc) info initial_state

tactic_list :: TypeEnvironment -> [Named Tactic]
tactic_list env =
  let
    vars = map (param_varN . fst) env
    bools = map prim_boolN [True, False]
    ints = map prim_intN [0..10]
    intros = map introN [variable_gen env]
    applys = map (f_applyN . fst) env
  in
    vars ++ bools ++ ints ++ intros ++ applys ++ [casesN]

  
show_complete :: [(Named Tactic, Expression)] -> String
show_complete pairs =
  let
    indexed = zip [0..] pairs
  in
    concatMap (\(n, (namedT, exp)) -> (show n) ++ ". " ++ (show exp) ++ " [ " ++ (name namedT) ++ " ]\n") indexed