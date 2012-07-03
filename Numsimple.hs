module Numsimple where

data Op = Plus | Minus | Mul | Div | Pow
          deriving (Eq, Show)

data SymbolicManip a =
    Number a | Symbol String |
    BinaryArith Op (SymbolicManip a) (SymbolicManip a) |
    UnaryArith String (SymbolicManip a)
    deriving (Eq)

instance Num a => Num (SymbolicManip a) where
    a + b = BinaryArith Plus a b
    a - b = BinaryArith Minus a b
    a * b = BinaryArith Mul a b
    negate a = BinaryArith Mul (Number (-1)) a
    abs a = UnaryArith "abs" a
    signum _ = error "signum is unimplemented"
    fromInteger i = Number (fromInteger i)
