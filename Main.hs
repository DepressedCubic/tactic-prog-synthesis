import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment
import System.IO

import AST
import Interpreter
import Parser
import Environment

data LoopState = TYPE_CHECK [TopLevel] [TopLevel] TypeEnvironment | 
             INTERPRET [TopLevel] ValueEnvironment TypeEnvironment |
             REPL ValueEnvironment TypeEnvironment

prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

loop :: LoopState -> IO ()
loop (TYPE_CHECK to_check checked types) = do
  case to_check of
    [] -> do
      putStrLn "Type checking complete!"
      putStrLn "Interpreting..."
      loop (INTERPRET (reverse checked) initial_environment types)
    (tp : tps) -> do
      case (type_check_error types tp) of
        Nothing -> loop (TYPE_CHECK tps (tp : checked) (update_types types tp))
        Just error ->
          case tp of
            LetRec (TypeAnnotation name _) _ -> do
              putStrLn ("Error when type checking <" ++ name ++ ">:")
              putStrLn error
              putStrLn ""
              putStrLn "Interpreting type checked code..."
              loop (INTERPRET (reverse checked) initial_environment types)
            Let (TypeAnnotation name _) _ -> do
              putStrLn ("Error when type checking <" ++ name ++ ">:")
              putStrLn error
              putStrLn ""
              putStrLn "Interpreting type checked code..."
              loop (INTERPRET (reverse checked) initial_environment types)

loop (INTERPRET to_interpret env types) = do
  case to_interpret of
    [] -> do
      putStrLn "Interpreted!"
      loop (REPL env types)
    (tp : tps) -> loop (INTERPRET tps (update_environment env tp) types)

loop (REPL env types) = do
  input <- prompt "> "
  if (take 4 input == "exit")
    then return ()
    else
      let 
        exp_str = unwords $ words input
      in
        case (parse (with_eof parse_expression) "" exp_str) of
          Left parse_err -> do
            print parse_err
            loop (REPL env types)
          Right exp -> do
            case (get_type exp types) of
              Right t -> putStrLn $ show_value $ (eval exp env TP)
              Left type_err -> putStrLn type_err
            loop (REPL env types)

main :: IO ()
main = do
  args <- getArgs
  if (null args)
    then putStrLn "Usage: ./Main <filename>"
    else do
      source <- readFile $ head args
      case (parse parse_code "" (unwords $ words source)) of
        Left err -> print err
        Right code ->
          loop (TYPE_CHECK code [] initial_types)