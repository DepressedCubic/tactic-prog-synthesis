import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment
import System.IO

import AST
import Interpreter
import Parser
import Environment

data LoopState = TYPE_CHECK [TopLevel] [TopLevel] TypeEnvironment | 
             INTERPRET [TopLevel] ValueEnvironment |
             REPL ValueEnvironment

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
      loop (INTERPRET (reverse checked) initial_environment)
    (tp : tps) -> do
      if (correct_type types tp)
        then loop (TYPE_CHECK tps (tp : checked) (update_types types tp))
        else
          case tp of
            LetRec (TypeAnnotation name _) _ -> do
              putStrLn ("Error when type checking <" ++ name ++ ">!")
              putStrLn "Interpreting type checked code..."
              loop (INTERPRET (reverse checked) initial_environment)
            Let (TypeAnnotation name _) _ -> do
              putStrLn ("Error when type checking <" ++ name ++ ">!")
              putStrLn "Interpreting type checked code..."
              loop (INTERPRET (reverse checked) initial_environment)

loop (INTERPRET to_interpret env) = do
  case to_interpret of
    [] -> do
      putStrLn "Interpreted!"
      loop (REPL env)
    (tp : tps) -> loop (INTERPRET tps (update_environment env tp))

loop (REPL env) = do
  input <- prompt "> "
  let
    keyword = head $ words input
    exp_str = unwords $ tail $ words input
  case keyword of
    "exit" -> return ()
    "eval" -> do
      case (parse parse_expression "" exp_str) of
        Left err -> do
          print err
          loop (REPL env)
        Right exp -> do
          putStrLn $ show_value $ (eval exp env TP)
          loop (REPL env)
    _ -> do
      putStrLn "Wrong keyword. Try again!"
      loop (REPL env)

main :: IO ()
main = do
  args <- getArgs
  source <- readFile $ head args
  case (parse parse_code "" (unwords $ words source)) of
    Left err -> print err
    Right code ->
      loop (TYPE_CHECK code [] initial_types)