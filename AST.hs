module AST where

-- Type Annotations

data PrimitiveType = INT | STRING | BOOL
  deriving (Show)

data Type = Func Type Type | List Type | PrimType PrimitiveType
  deriving (Show)

data TypeAnnotation = TypeAnnotation String Type
  deriving (Show)

-- Values

data PrimitiveValue = Int Integer | Bool Bool
  deriving (Show)

data Value = Value PrimitiveValue

-- Expressions

data InbuiltBinary = ADD | SUB | MUL | DIV | AND | OR
  deriving (Show)

data Expression =
  App Expression Expression |
  Var String |
  Ifte Expression Expression Expression |
  Lambda TypeAnnotation Expression |
  Num Integer | Boolean Bool |
  Binary InbuiltBinary Expression Expression
  deriving (Show)


data TopLevel = 
  Let TypeAnnotation Expression | LetRec TypeAnnotation Expression
  deriving (Show)