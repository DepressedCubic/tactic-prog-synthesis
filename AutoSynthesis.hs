import System.IO
import Text.Parsec
import Data.Maybe
import Text.Read

import Parser
import AST
import Environment
import Synthesis2
import Complete

-- REPL

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- Performs a tactic
do_tactic :: Tactic -> S -> S
do_tactic tactic s =
  let
    (local_env, t) = fromJust (global_find_type_env (env s) (hole s) (code s))
    (m_exp, s') = tactic (local_env, t) s
  in
    case m_exp of
      Nothing -> s
      Just (e, _) ->
        s'{code = code_fill e (hole s) (code s')}


choices :: S -> [(Named Tactic, Expression)]
choices s =
  case (global_find_type_env (env s) (hole s) (code s)) of
    Just (local_env, t) -> complete (local_env, t) (tactic_list local_env)
    _ -> []

-- Possible tactics

read_action :: S -> IO ()
read_action s = do
  input <- prompt "> "
  case (words input) of
    ["exit"] -> return ()
    ("lt" : name : type_str) ->
      case (parse parse_type "" (unwords type_str)) of
        Left err -> do
          print err
          read_action s
        Right t ->
          let
            t' = names_to_ids (zip (get_type_names t) [0..]) t
            (next', t'') = shift_ids (next_type s, t')
            new = (Let (TypeAnnotation name t'') (IDHole (next_id s) t''))
            s' = s{code = (new : (code s)), env = ((name, t'') : (env s)), next_id = (next_id s) + 1, hole = (next_id s), next_type = next'}
          in
            display s'
    ("ltrc" : name : type_str) ->
      case (parse parse_type "" (unwords type_str)) of
        Left err -> do
          print err
          read_action s
        Right t ->
          let
            t' = names_to_ids (zip (get_type_names t) [0..]) t
            (next', t'') = shift_ids (next_type s, t')
            new = (LetRec (TypeAnnotation name t'') (IDHole (next_id s) t''))
            s' = s{code = (new : (code s)), env = ((name, t'') : (env s)), next_id = (next_id s) + 1, hole = (next_id s), next_type = next'}
          in
            display s'
    ["var", name] ->
      display (do_tactic (param_var name) s)
    ["intro", name] ->
      display (do_tactic (intro name) s)
    ["cases"] ->
      display (do_tactic cases s)
    ["apply", name] ->
      display (do_tactic (f_apply name) s)
    ["int", int_str] ->
      display (do_tactic (prim_int (read int_str)) s)
    ["jump", int_str] ->
      display s{hole = (read int_str)}
    [] ->
      case (choices s) of
        [] -> do
          putStrLn "No tactics available."
          read_action s
        (x : xs) -> display (do_tactic (run $ fst x) s)
    [int_str] ->
      case (readMaybe int_str) of
        Nothing -> do
          putStrLn "Unknown action. Try again."
          read_action s
        Just n -> display (do_tactic (run $ fst ((choices s) !! n)) s)
    _ -> do
      putStrLn "Unknown action. Try again."
      read_action s

display :: S -> IO ()
display s = do
  putStrLn (show_code $ reverse $ code s)
  putStrLn ("HOLE: " ++ (show (hole s)))
  putStrLn (show_complete (choices s))
  read_action s

main :: IO ()
main = do
  read_action initial_state