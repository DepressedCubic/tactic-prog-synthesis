module AST where

{-
Data types for AST representation of types and type annotations
Type 'T' represents a placeholder type, for polymorphic functions such
as cons : T -> [T] -> [T]. Currently, the user is not able to define
any polymorphic functions (and the parser totally ignores the existence
of these placeholders).

The type 'Any' is a special case used only for the 'fst' and 'snd' functions;
It is a sort of placeholder that is not supposed to be instantiated and
matches to anything.
-}
data PrimitiveType = INT | BOOL
  deriving (Show, Eq)

data Type = Func Type Type | List Type | PrimType PrimitiveType | T
            | Any | Prod Type Type
  deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation String Type
  deriving (Show)

{- 
Data type for values: that is, objects that have been 'interpreted'
and are ready for use.
The special value 'Recurse' is used for a recursively defined function
when we're in the process of evaluating it.
-}
data Value = 
  Int Integer | 
  Bool Bool | 
  Function (Value -> Value) | 
  ListValue [Value] |
  PairValue (Value, Value) |
  Recurse
  
{-
Data types for expressions and top-levels; that is, the AST 
representation of the source code without any interpretation.
-}
data Expression =
  App Expression Expression |
  Var String |
  Ifte Expression Expression Expression |
  Lambda TypeAnnotation Expression |
  Pair Expression Expression |
  Num Integer | 
  Boolean Bool
  deriving (Show)


data TopLevel = 
  Let TypeAnnotation Expression | LetRec TypeAnnotation Expression | TP
  deriving (Show)

{-
Auxiliary functions to print types and values. Note that functions are not
fully represented.
-}
show_type :: Type -> String
show_type T = "T"
show_type (PrimType INT) = "Int"
show_type (PrimType BOOL) = "Bool"
show_type (List t) = "[" ++ (show_type t) ++ "]"
show_type (Func r s) = "(" ++ (show_type r) ++ " -> " ++ (show_type s) ++ ")"
show_type (Prod r s) = "<" ++ (show_type r) ++ ", " ++ (show_type s) ++ ">"
show_type Any = "*"

show_value :: Value -> String
show_value (Int n) = show n
show_value (Bool b) = show b
show_value (Function f) = "(Function _)"
show_value (Recurse) = "Recurse"
show_value (ListValue l) =
  "[" ++ (unwords $ map show_value l) ++ "]"
show_value (PairValue (r, s)) =
  "<" ++ (show_value r) ++ ", " ++ (show_value s) ++ ">"