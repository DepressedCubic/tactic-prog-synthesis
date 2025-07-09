import Text.Parsec
import Text.Parsec.String (Parser)
import System.Environment
import System.IO

import AST
import Parser
import Environment

-- Tactics

type Tactic = TypeEnvironment -> Type -> [Expression]

{-
Given _ : T, gets all variables of type T.
[Variable synthesis]
-}
var :: Tactic
var env t = 
  map (to_var . fst) $ filter (snd x == t) env
  where
    to_var s = (Var s)

-- [Boolean synthesis]
prim_bool :: Bool -> Tactic
prim_bool b _ t =
  case t of
    PrimType BOOL -> [(Boolean b)]
    _ -> []

-- [Integer synthesis]
prim_int :: Integer -> Tactic
prim_int n _ t =
  case t of
    PrimType INT -> [(Num n)]
    _ -> []

{-
Given x : T and _ : T -> U, synthesizes
\(x : T) -> (_ : U).
[Lambda synthesis]
-}
intro :: String -> Tactic
intro x env t =
  case (lookup x env, t) of
    (Just t, Func t u) -> [(Lambda (TypeAnnotation x t) (Hole u))]
    _ -> []

{-
Given type T and _ : U, synthesizes
(_ : T -> U) (_ : T).
[Application synthesis]
-}
apply :: Type -> Tactic
apply t _ u = [(App (Hole (Func t u)) (Hole t))]

{-
Given _ : T, synthesizes
if (_ : Bool) then (_ : T) else ( : T).
[Conditional synthesis]
-}
cases :: Tactic
cases _ t = [(Ifte (Hole Bool) (Hole t) (Hole t))]

-- "Generalized tactic"
-- complete :: [Tactic] -> Type -> [Expression]


fill :: Expression -> Int -> Expression -> Expression
fill (Boolean b) id filler = (Boolean b)
fill (Num n) id filler = (Num n)
fill (Pair e1 e2) id filler = (Pair (fill e1 id filler) (fill e2 id filler))
fill (Lambda t e) = (Lambda t (fill e id filler))
fill (Ifte cond e1 e2) = (Ifte 
  (fill cond id filler) 
  (fill e1 id filler) 
  (fill e2 id filler))
fill (Var s) = (Var s)
fill (App e1 e2) id filler = (App (fill e1 id filler) (fill e2 id filler))
fill (Hole id' t)
  | id == id' = filler
  | otherwise = (Hole id' t)


main :: IO ()
main = do
  args <- getArgs
  if (null args)
    then putStrLn "Usage: ./Synthesis <filename>"
    else do
      source <- readFile $ head args
      case (parse parse_code "" (unwords $ words source)) of
        Left err -> print err
        Right code -> putStr (show_code code)