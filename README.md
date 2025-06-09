# typed-lambda

A parser and interpreter for a small functional language with type checking written in Haskell, to be submitted as a semester project for the Non-Procedural Programming class in Charles University.

## Current Features
- Type checking before interpretation
- Support for primitive types (booleans and arbitrary-precision integers)
- Functions, pairs, lists (and functions to handle them)
- Capability to define functions recursively.

## How to Run
- It is assumed that GHC has been installed.
- Clone this repository and, inside it, run in the terminal:
```
ghc Main.hs
```
- After compilation, one can interpret a file ```filename```
as follows:
```
./Main filename
```
- After this, the REPL will start.
## Syntax

All variable and function names must be alphabetic.

### Type Annotations

- We have primitive types ```Int``` (integers) and ```Bool``` (booleans).
- For any types ```U``` and ```V```, we can write the types ```U -> V``` (function from ```U``` to ```V```), ```[U]``` (list with elements of type ```U```) and ```<U, V>``` (pair with components of type ```U``` and ```V```).

**Example:** ```[Int] -> [Bool]```, ```[<Bool,Int> -> [Bool]]```

- Then, a full type annotation is of the form ```(name : type)``` (including the parens), where ```name``` is a valid name and ```type``` is a valid type.

### Top-Level Definitions

- For non-recursive definitions we use ```let``` and for recursive function definitions we use ```letrec```, as follows:
```
let (var : type) = ... expression ...
```
```
letrec (var : type) = ... expression using var ...
```
- It is important to note that right now, the only type of recursion is that for recursively defined functions; and even there, one shouldn't apply a function to them
in their recursive definition (so for example, ```cons f nil``` cannot appear in the recursive definition of ```f```).

### Expressions
- Integers are represented by their decimal expansions, and Booleans by ```True``` and ```False```.
- To apply arguments ```x1``` , ... , ```xn``` to function ```f```, it suffices to write ```f x1 ... xn```, which is interpreted as ```((f x1) ... xn)```. To resolve ambiguities the usage of parens is advised.
- To introduce a function from a variable ```x``` of type ```U``` to some type ```V```, we write:
```
\(x : U) -> ... expression of type V ...
```
- An if/then/else (conditional) expression is written as:
```
if (cond) then (exp1) else (exp2)
```
where ```cond```, ```exp1```, and ```exp2``` are expressions. Currently, the parens are mandatory.
- A pair of expressions ```exp1``` and ```exp2``` is written as ```<exp1, exp2>```.

## Using the REPL
- To evaluate an expression ```exp``` (that may include some of the interpreted variables), it suffices to write ```exp```.
- To exit the REPL, use ```exit```. This is the only REPL keyword.

## Built-in Objects
1. ```nil : [T]```: The empty list.
2. ```cons : T -> [T] -> [T]```: Takes an element and a list and prepends the element to the list (necessary to build lists).
3. ```head : [T] -> T```: Takes a list and returns its head, but be careful, doing so on the empty list may cause an error.
4. ```tail : [T] -> [T]```: Takes a list and returns its tail.
5. ```isnil : [T] -> Bool```: Takes a list and evaluates to ```True``` exactly if the list is empty.
6. ```leq : Int -> Int -> Bool```: Takes in two integers and evaluates to ```True``` if the first is less than or equal to the second.
7. ```eq : Int -> Int -> Bool```: Takes in two integers and evaluates to ```True``` if they are equal.
8. ```and : Bool -> Bool -> Bool```: Logical AND.
9. ```or : Bool -> Bool -> Bool```: Logical OR.
10. ```not : Bool -> Bool```: Logical NOT.
11. ```add : Int -> Int -> Int```: Integer addition.
12. ```sub : Int -> Int -> Int```: Integer subtraction.
13. ```mul : Int -> Int -> Int```: Integer multiplication.
14. ```div : Int -> Int -> Int```: Integer division.
15. ```zero : Int -> Bool```: Takes in an integer and return ```True``` exactly if it's zero.
16. ```fst : <T, _> -> T```: Takes in a pair and returns the first component.
17. ```snd : <_, T> -> T```: Takes in a pair and returns the second component.

## Sample File
A file with sample code is provided: ```sample.lc```. It includes an implementation of insertion sort, and some other functions (including a not well-typed one!)