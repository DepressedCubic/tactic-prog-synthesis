import Data.Maybe
import AST

type TypeEnvironment = [(String, Type)]

type ValueEnvironment = [(String, Value)]

nil_val :: Value
nil_val = ListValue []

cons_val :: Value -> Value
cons_val v =
  Function (\(ListValue l) -> (ListValue (v:l)))

initial_types :: TypeEnvironment
initial_types =
  [
    ("nil", )
    ("cons",)
  ]

initial_environment :: ValueEnvironment
initial_environment =
  [
    ("nil", nil_val),
    ("cons", Function cons_val)
  ]

-- For some expression, returns either Just T if it's
-- well-typed and of type T, or Nothing.
get_type :: Expression -> TypeEnvironment -> Maybe Type
get_type (App e1 e2) env = do
  t1 <- get_type e1 env
  t2 <- get_type e2 env
  case t1 of
    Func r s -> if (r == t2) then return (s) else Nothing
    _ -> Nothing
get_type (Var s) env = lookup s env
get_type (Ifte c e1 e2) env = do
  t <- get_type c env
  t1 <- get_type e1 env
  t2 <- get_type e2 env
  if ((t == PrimType BOOL) && (t1 == t2))
    then return (t1)
    else Nothing
get_type (Lambda (TypeAnnotation name t1) exp) env = do
  t2 <- get_type exp ((name, t1) : env)
  return (Func t1 t2)
get_type (Num n) env = Just (PrimType INT)
get_type (Boolean b) env = Just (PrimType BOOL)

-- values can be Haskell ints, bools, lists, or functions!
eval :: Expression -> ValueEnvironment -> TopLevel -> Value
eval (App e1 e2) env tp =
  let
    v1 = eval e1 env tp
    v2 = eval e2 env tp
  in
    case v1 of
      Recurse -> case tp of
        LetRec _ (Lambda (TypeAnnotation name t) low_exp) ->
          eval low_exp ((name, v2) : env) tp
      Function f -> f v2
eval (Var s) env tp = fromJust (lookup s env)
eval (Ifte cond e1 e2) env tp =
  let
    cond_val = eval cond env tp
  in
    case cond_val of
      Bool True -> eval e1 env tp
      Bool False -> eval e2 env tp
eval (Lambda (TypeAnnotation name t) e) env tp =
  Function (\x -> eval e ((name, x) : env) tp)
eval (Num n) env tp = Int n
eval (Boolean b) env tp = Bool b 

update_environment :: TopLevel -> ValueEnvironment -> ValueEnvironment
update_environment (Let (TypeAnnotation x t) e) env =
  (x, eval e env tp) : env
  where
    tp = (Let (TypeAnnotation x t) e)
update_environment (LetRec (TypeAnnotation x t) e) env =
  (x, eval e ((x, Recurse) : env) tp) : env
  where
    tp = (LetRec (TypeAnnotation x t) e)

-- what about letrec? think about it with paper...