module AST where

import Data.Maybe
import Data.List

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
show_type (TypeVar n) = "T(" ++ (show n) ++ ")"
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

-- Type unification

type Substitution = [(Int, Type)]

apply_sub :: Substitution -> Type -> Type
apply_sub sub (TypeVar n) =
  case (lookup n sub) of
    Just t -> t
    _ -> (TypeVar n)
apply_sub sub (Func t1 t2) = Func (apply_sub sub t1) (apply_sub sub t2)
apply_sub sub (List t) = List (apply_sub sub t)
apply_sub sub (Prod t1 t2) = Prod (apply_sub sub t1) (apply_sub sub t2)
apply_sub sub (PrimType t) = PrimType t

compose :: Substitution -> Substitution -> Substitution
compose s1 s2 =
  let
    is_trivial (n, t) =
      case t of
        TypeVar n -> True
        _ -> False
    s1' = map (\(n, t) -> (n, apply_sub s2 t)) s1
    s2' = filter (\(n, _) -> not $ isJust $ lookup n s1) s2
    s1'' = filter (\p -> not $ is_trivial p) s1'
  in
    union s1'' s2'



unify :: Substitution -> Type -> Type -> Maybe Substitution
unify sub t1 t2 =
  let
    t1' = if (is_var t1) then (apply_sub sub t1) else t1
    t2' = if (is_var t2) then (apply_sub sub t2) else t2
  in
    if ((is_var t1') && (t1' == t2'))
      then Just sub
    else
      case (t1', t2') of
        (Func u v, Func u' v') -> do
          sub' <- unify sub u u'
          sub'' <- unify sub' v v'
          return (sub'')
        (Func _ _, Prod _ _) -> Nothing
        (Func _ _, List _) -> Nothing
        (Func _ _, PrimType _) -> Nothing
        (Prod u v, Prod u' v') -> do
          sub' <- unify sub u u'
          sub'' <- unify sub' v v'
          return (sub'')
        (Prod _ _, Func _ _) -> Nothing
        (Prod _ _, List _) -> Nothing
        (Prod _ _, PrimType _) -> Nothing
        (List t, List t') -> unify sub t t'
        (List _, Func _ _) -> Nothing
        (List _, Prod _ _) -> Nothing
        (List _, PrimType _) -> Nothing
        (PrimType u, PrimType v) -> 
          if (u == v) 
            then Just sub 
            else Nothing
        (PrimType _, Func _ _) -> Nothing
        (PrimType _, Prod _ _) -> Nothing
        (PrimType _, List _) -> Nothing
        _ ->
          if (not $ is_var t1')
            then unify sub t2' t1'
            else
              case t1' of
                (TypeVar n) ->
                  if (occurs n t2')
                    then Nothing
                    else Just (compose sub [(n, t2')])
                _ -> Nothing
  where
    is_var (TypeVar n) = True
    is_var _ = False
    occurs n (TypeVar m) = (n == m)
    occurs n (Func t1 t2) = (occurs n t1) || (occurs n t2)
    occurs n (Prod t1 t2) = (occurs n t1) || (occurs n t2)
    occurs n (List t) = occurs n t
    occurs n _ = False

