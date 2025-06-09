module Parser where

import System.Environment
import Data.Maybe
import Data.Functor
import Text.Parsec
import Text.Parsec.String (Parser)
import Control.Applicative (many)
import AST

{-
NOTE: When debugging these parsers in the terminal, make sure to
use \\ for \ (in lambda expressions).
-}

keywords :: [String]
keywords =
  [
    "if",
    "then",
    "else",
    "let",
    "letrec"
  ]

with_eof :: Parser a -> Parser a
with_eof p = do
  t <- p
  eof
  return (t)

chr :: Char -> Parser Char
chr c = do
  spaces
  t <- char c
  spaces
  return (t)

str :: String -> Parser String
str s = do
  spaces
  t <- string s
  spaces
  return (t)

parens :: Parser a -> Parser a
parens p = do
  chr '('
  t <- p
  chr ')'
  return (t)

parse_name :: Parser String
parse_name = do
  word <- many1 letter
  spaces
  if (elem word ["let", "letrec"])
    then fail "used keyword in wrong place"
    else return (word)

parse_natural :: Parser Integer
parse_natural = do
  n <- many1 digit
  spaces
  return (read n)

-- Parsers for primitive types (ints and bools)

p_int :: Parser Type
p_int = str "Int" $> PrimType INT

p_bool :: Parser Type
p_bool = str "Bool" $> PrimType BOOL

-- Parsers for general types (and type annotations)

parse_prim_type :: Parser Type
parse_prim_type = p_int <|> p_bool

parse_list_type :: Parser Type
parse_list_type = do
  chr '['
  t <- parse_type
  chr ']'
  return (List t)

parse_type :: Parser Type
parse_type = do
  t1 <- parse_pair_type <|> parse_prim_type <|> parse_list_type <|> 
        between (chr '(') (chr ')') parse_type
  is_func <- optionMaybe (str "->")
  case is_func of
    Nothing -> return (t1)
    Just x -> do
      spaces
      t2 <- parse_type
      return (Func t1 t2)


parse_type_annotation :: Parser TypeAnnotation
parse_type_annotation = do
  chr '('
  n <- parse_name
  chr ':'
  t <- parse_type
  chr ')'
  return (TypeAnnotation n t)

parse_pair_type :: Parser Type
parse_pair_type = do
  chr '<'
  u <- parse_type
  chr ','
  v <- parse_type
  chr '>'
  return (Prod u v)

-- Parsers for specific types of expressions

parse_pair :: Parser Expression
parse_pair = do
  chr '<'
  u <- parse_expression
  chr ','
  v <- parse_expression
  chr '>'
  return (Pair u v)


parse_lambda :: Parser Expression
parse_lambda = do
  str "\\"
  t <- parse_type_annotation
  str "->"
  exp <- parse_expression
  return (Lambda t exp)

parse_ifte :: Parser Expression
parse_ifte = do
  str "if"
  cond <- parens parse_expression
  str "then"
  if_true <- parens parse_expression
  str "else"
  if_false <- parens parse_expression
  return (Ifte cond if_true if_false)

parse_integer :: Parser Expression
parse_integer = (do
  chr '-'
  n <- parse_natural
  return (Num (-n)))
  <|> (do
    n <- parse_natural
    return (Num n)
  )

parse_bool :: Parser Expression
parse_bool = (str "False" $> Boolean False) 
         <|> (str "True" $> Boolean True)

parse_prim_value :: Parser Expression
parse_prim_value = do
  p <- parse_integer <|> parse_bool
  return (p)

parse_variable :: Parser Expression
parse_variable = do
  name <- parse_name
  if (elem name keywords)
    then fail $ "Syntax error: Expected variable but got keyword <" ++ name ++ ">"
    else return (Var name)

{-
app_builder takes a list of expressions [e1, e2, e3, ...]
and turns them into a single expression with applications:
(App (App (App ... e3) e2) e1)
-}
app_builder :: [Expression] -> Expression
app_builder exps = 
  case exps of
      [x] -> x
      (x : xs) -> (App (app_builder xs) x)

-- Parsers for general expressions and top-levels
parse_single_expression :: Parser Expression
parse_single_expression =
        try parse_pair <|>
        try parse_lambda <|> 
        try parse_prim_value <|> 
        try parse_ifte <|> 
        try parse_variable

parse_expression :: Parser Expression
parse_expression = do
  e_list <- many1 (try (parens parse_expression) <|> parse_single_expression)
  if (null e_list)
    then fail "no expressions found"
    else return (app_builder (reverse e_list))

parse_top_level :: Parser TopLevel
parse_top_level = do
  keyword <- many1 letter
  t <- parse_type_annotation
  chr '='
  e <- parse_expression
  case keyword of
    "let" -> return (Let t e)
    "letrec" -> return (LetRec t e)
    _ -> fail "Syntax error: Expected top-level function definition"

parse_code :: Parser [TopLevel]
parse_code = many1 parse_top_level

