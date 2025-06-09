module Environment where

import AST

{-
In this module, lies everything related to the type environments and
the runtime value environments; this includes the definition of built-in
functions and the initial type and value environments.
-}

type TypeEnvironment = [(String, Type)]

type ValueEnvironment = [(String, Value)]

nil_val :: Value
nil_val = ListValue []

cons_val :: Value
cons_val =
  Function (\v -> Function (\(ListValue l) -> (ListValue (v:l))))

head_val :: Value
head_val =
  Function (\(ListValue l) -> (head l))

tail_val :: Value
tail_val =
  Function (\(ListValue l) -> ListValue (tail l))

isnil_val :: Value
isnil_val =
  Function (\(ListValue l) -> (Bool (null l)))

leq_val :: Value
leq_val =
  Function (\(Int v) -> (Function (\(Int w) -> Bool (v <= w))))

eq_val :: Value
eq_val =
  Function (\(Int v) -> (Function (\(Int w) -> Bool (v == w))))

and_val :: Value
and_val =
  Function (\(Bool b1) -> (Function (\(Bool b2) -> Bool (b1 && b2))))

or_val :: Value
or_val =
  Function (\(Bool b1) -> (Function (\(Bool b2) -> Bool (b1 || b2))))

not_val :: Value
not_val =
  Function (\(Bool b) -> (Bool (not b)))

add_val :: Value
add_val =
  Function (\(Int x1) -> (Function (\(Int x2) -> Int (x1 + x2))))

sub_val :: Value
sub_val =
  Function (\(Int x1) -> (Function (\(Int x2) -> Int (x1 - x2))))

mul_val :: Value
mul_val =
  Function (\(Int x1) -> (Function (\(Int x2) -> Int (x1 * x2))))

div_val :: Value
div_val =
  Function (\(Int x1) -> (Function (\(Int x2) -> Int (div x1 x2))))

zero_val :: Value
zero_val =
  Function (\(Int x) -> (Bool (x == 0)))

fst_val :: Value
fst_val =
  Function (\(PairValue (a, b)) -> a)

snd_val :: Value
snd_val =
  Function (\(PairValue (a, b)) -> b)

initial_types :: TypeEnvironment
initial_types =
  [
    ("nil", List T),
    ("cons", Func T (Func (List T) (List T))),
    ("head", Func (List T) T),
    ("tail", Func (List T) (List T)),
    ("isnil", Func (List T) (PrimType BOOL)),
    ("leq", Func (PrimType INT) (Func (PrimType INT) (PrimType BOOL))),
    ("eq", Func (PrimType INT) (Func (PrimType INT) (PrimType BOOL))),
    ("and", Func (PrimType BOOL) (Func (PrimType BOOL) (PrimType BOOL))),
    ("or", Func (PrimType BOOL) (Func (PrimType BOOL) (PrimType BOOL))),
    ("not", Func (PrimType BOOL) (PrimType BOOL)),
    ("add", Func (PrimType INT) (Func (PrimType INT) (PrimType INT))),
    ("sub", Func (PrimType INT) (Func (PrimType INT) (PrimType INT))),
    ("mul", Func (PrimType INT) (Func (PrimType INT) (PrimType INT))),
    ("div", Func (PrimType INT) (Func (PrimType INT) (PrimType INT))),
    ("zero", Func (PrimType INT) (PrimType BOOL)),
    ("fst", Func (Prod T Any) T),
    ("snd", Func (Prod Any T) T)
  ]

initial_environment :: ValueEnvironment
initial_environment =
  [
    ("nil", nil_val),
    ("cons", cons_val),
    ("head", head_val),
    ("tail", tail_val),
    ("isnil", isnil_val),
    ("leq", leq_val),
    ("eq", eq_val),
    ("and", and_val),
    ("or", or_val),
    ("not", not_val),
    ("add", add_val),
    ("sub", sub_val),
    ("mul", mul_val),
    ("div", div_val),
    ("zero", zero_val),
    ("fst", fst_val),
    ("snd", snd_val)
  ]