module AST where

-- Type Annotations

data PrimitiveType = INT | BOOL
  deriving (Show, Eq)

data Type = Func Type Type | List Type | PrimType PrimitiveType | T
  deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation String Type
  deriving (Show)

-- Values


data Value = 
  Int Integer | 
  Bool Bool | 
  Function (Value -> Value) | 
  ListValue [Value] | 
  Recurse
  
-- Expressions

data InbuiltBinary = ADD | SUB | MUL | DIV | AND | OR
  deriving (Show)

data Expression =
  App Expression Expression |
  Var String |
  Ifte Expression Expression Expression |
  Lambda TypeAnnotation Expression |
  Num Integer | 
  Boolean Bool
  deriving (Show)


data TopLevel = 
  Let TypeAnnotation Expression | LetRec TypeAnnotation Expression | TP
  deriving (Show)

show_type :: Type -> String
show_type T = "T"
show_type (PrimType INT) = "Int"
show_type (PrimType BOOL) = "Bool"
show_type (List t) = "[" ++ (show_type t) ++ "]"
show_type (Func r s) = "(" ++ (show_type r) ++ " -> " ++ (show_type s) ++ ")"

show_value :: Value -> String
show_value (Int n) = show n
show_value (Bool b) = show b
show_value (Function f) = "(Function _)"
show_value (Recurse) = "Recurse"
show_value (ListValue l) =
  "[" ++ (unwords $ map show_value l) ++ "]"