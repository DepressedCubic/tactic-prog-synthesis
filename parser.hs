-- module Parser where

import System.Environment
import Data.Maybe
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (many)
import AST

-- note: do not forget to use "\\" when debugging in terminal!

parens :: Parser a -> Parser a
parens p = do
  char '('
  spaces
  t <- p
  spaces
  char ')'
  spaces
  return (t)


parse_name :: Parser String
parse_name = do
  word <- many1 letter
  if (elem word ["add", "mul", "sub", "div", "and", "or", "let", "letrec"])
    then fail "used keyword in wrong place"
    else return (word)

parse_natural :: Parser Integer
parse_natural = do
  n <- many1 digit
  return (read n)

parse_binary_op :: Parser InbuiltBinary
parse_binary_op = do
  str <- many1 letter
  case str of
    "add" -> return (ADD)
    "mul" -> return (MUL)
    "sub" -> return (SUB)
    "div" -> return (DIV)
    "and" -> return (AND)
    "or" -> return (OR)
    _ -> fail "expected binary operator"


-- PrimitiveType parsing

p_int :: Parser Type
p_int = do
  string ("Int")
  return (PrimType INT)

p_str :: Parser Type
p_str = do
  string ("String")
  return (PrimType STRING)

p_bool :: Parser Type
p_bool = do
  string ("Bool")
  return (PrimType BOOL)

-- Type parsing

parse_prim_type :: Parser Type
parse_prim_type = p_int <|> p_str <|> p_bool

parse_list_type :: Parser Type
parse_list_type = do
  char '['
  spaces
  t <- parse_type
  spaces
  char ']'
  return (List t)

parse_type :: Parser Type
parse_type = do
  t1 <- parse_prim_type <|> parse_list_type <|> 
        between (char '(') (char ')') parse_type
  spaces
  is_func <- optionMaybe (string "->")
  case is_func of
    Nothing -> return (t1)
    Just x -> do
      spaces
      t2 <- parse_type
      return (Func t1 t2)


parse_type_annotation :: Parser TypeAnnotation
parse_type_annotation = do
  char '('
  spaces
  n <- parse_name
  spaces
  char ':'
  spaces
  t <- parse_type
  spaces
  char ')'
  return (TypeAnnotation n t)

-- Expression parsing

parse_lambda :: Parser Expression
parse_lambda = do
  string "\\"
  t <- parse_type_annotation
  spaces
  string "->"
  spaces
  exp <- parse_expression
  spaces
  return (Lambda t exp)

parse_ifte :: Parser Expression
parse_ifte = do
  string "if"
  spaces
  cond <- parens parse_expression
  spaces
  string "then"
  spaces
  if_true <- parens parse_expression
  spaces
  string "else"
  spaces
  if_false <- parens parse_expression
  spaces
  return (Ifte cond if_true if_false)

parse_integer :: Parser Expression
parse_integer = (do
  char '-'
  n <- parse_natural
  return (Num (-n)))
  <|> (do
    n <- parse_natural
    return (Num n)
  )

parse_bool :: Parser Expression
parse_bool = do
  string "False"
  return (Boolean False)
 <|> do
  string "True"
  return (Boolean True)



parse_prim_value :: Parser Expression
parse_prim_value = do
  p <- parse_integer <|> parse_bool
  spaces
  return (p)

parse_variable :: Parser Expression
parse_variable = do
  name <- parse_name
  spaces
  return (Var name)

parse_binary :: Parser Expression
parse_binary = do
  op <- parse_binary_op
  e1 <- parse_expression
  e2 <- parse_expression
  return (Binary op e1 e2)

app_builder :: [Expression] -> Maybe Expression
app_builder exps = 
  case exps of
      [] -> Nothing
      [x] -> Just x
      (x : xs) -> Just (App (fromJust (app_builder xs)) x)


parse_single_expression :: Parser Expression
parse_single_expression = 
        try parse_lambda <|> 
        try parse_prim_value <|> 
        try parse_ifte <|> 
        try parse_variable

parse_expression :: Parser Expression
parse_expression = do
  e_list <- many1 (try (parens parse_expression) <|> parse_single_expression)
  if (null e_list)
    then fail "no expressions found"
    else return (fromJust (app_builder (reverse e_list)))

parse_top_level :: Parser TopLevel
parse_top_level = do
  keyword <- many1 letter
  spaces
  t <- parse_type_annotation
  spaces
  char '='
  spaces
  e <- parse_expression
  case keyword of
    "let" -> return (Let t e)
    "letrec" -> return (LetRec t e)
    _ -> fail "expected top-level function definition"

parse_code :: Parser [TopLevel]
parse_code = many1 parse_top_level

{-
main = do
  args <- getArgs
  code <- readFile (head args)
  case (parse parse_code "" (unwords $ words code)) of
    Left err -> print err
    Right xs -> print xs
-}