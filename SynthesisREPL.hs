import System.IO
import Text.Parsec
import Data.Maybe

import Parser
import AST
import Environment
import Synthesis

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

-- Provisional
do_tactic :: String -> TypeEnvironment -> Tactic -> ([TopLevel], Int) -> IO ()
do_tactic id_str env tactic (code, next_id) =
  let
    id = read id_str
    (t, local_env) = fromJust (global_find_type_env env id code)
    m_exp = tactic local_env t
  in
    case m_exp of
      Nothing -> read_action code env next_id
      Just e ->
        let
          (e_id, next_id') = add_ids (e, next_id)
          (code', next_id'') = code_fill e_id id (code, next_id')
        in
          display_code code' env next_id''

read_action :: [TopLevel] -> TypeEnvironment -> Int -> IO ()
read_action code env next_id = do
  input <- prompt "> "
  case (words input) of
    ["exit"] -> return ()
    ("lt" : name : type_string) ->
      case (parse parse_type "" (unwords type_string)) of
        Left err -> do
          print err
          read_action code env next_id
        Right t ->
          let
            new = (Let (TypeAnnotation name t) (IDHole (next_id) t))
          in
            display_code (new : code) ((name, t) : env) (next_id + 1)
    ("ltrc" : name : type_string) ->
      case (parse parse_type "" (unwords type_string)) of
        Left err -> do
          print err
          read_action code env next_id
        Right t ->
          let
            new = (LetRec (TypeAnnotation name t) (IDHole (next_id) t))
          in
            display_code (new : code) ((name, t) : env) (next_id + 1)
    [id_str, "var", name] -> 
      do_tactic id_str env (param_var name) (code, next_id)
    [id_str, "intro", name] ->
      do_tactic id_str env (intro name) (code, next_id)
    [id_str, "cases"] ->
      do_tactic id_str env cases (code, next_id)
    [id_str, "apply", name] ->
      do_tactic id_str env (f_apply name) (code, next_id)

    _ -> do
      putStrLn "Unknown action. Try again."
      read_action code env next_id

-- Note; here the code is stored in reverse order, so we
-- must reverse it during display.
display_code :: [TopLevel] -> TypeEnvironment -> Int -> IO ()
display_code code env next_id = do
  putStrLn (show_code $ reverse code)
  read_action code env next_id

main :: IO ()
main = do
  read_action [] initial_types 0