module AST where

-- Type Annotations

data PrimitiveType = INT | BOOL
  deriving (Show, Eq)

data Type = Func Type Type | List Type | PrimType PrimitiveType
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
  Let TypeAnnotation Expression | LetRec TypeAnnotation Expression
  deriving (Show)