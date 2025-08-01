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
            | Any | Prod Type Type | TypeVar Int
  deriving (Show, Eq)

data TypeAnnotation = TypeAnnotation String Type

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
  Boolean Bool |
  IDHole Int Type |
  Hole Type

data TopLevel = 
  Let TypeAnnotation Expression | LetRec TypeAnnotation Expression | TP

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


tab :: String -> String
tab s = unlines $ map (\l -> "  " ++ l) $ lines s

instance Show TypeAnnotation where
  show (TypeAnnotation var t) = var ++ " : " ++ (show_type t)

instance Show Expression where
  show (Boolean b) = show b
  show (Num n) = show n
  show (Pair e1 e2) = "< " ++ show e1 ++ ", " ++ show e2 ++ " >"
  show (Lambda t e) =
    "\\(" ++ (show t) ++ ") -> (\n"
    ++ tab (show e) ++ ")"
  show (Ifte cond e1 e2) =
    "if (" ++ (show cond) ++ ")\n" ++
    tab ("then (" ++ (show e1) ++ ")\n"
      ++ "else (" ++ (show e2) ++ ")")
  show (Var s) = s
  show (App e1 e2) = "(" ++ (show e1) ++ " " ++ (show e2) ++ ")"
  show (IDHole id t) = "(_" ++ (show id) ++ " : " ++ (show_type t) ++ ")"
  show (Hole t) = "(_" ++ " : " ++ (show_type t) ++ ")"

instance Show TopLevel where
  show (Let t e) = "let (" ++ (show t) ++ ") =\n" ++ (tab $ show e)
  show (LetRec t e) = "letrec (" ++ (show t) ++ ") =\n" ++ (tab $ show e)

show_code :: [TopLevel] -> String
show_code code = unlines $ map show code

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