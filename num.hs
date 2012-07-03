import Numsimple
import Data.List (intersperse)

instance (Fractional a) => Fractional (SymbolicManip a) where
    a / b = BinaryArith Div a b
    recip a = BinaryArith Div (Number 1) a
    fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
    pi = Symbol "pi"
    exp a = UnaryArith "exp" a
    log a = UnaryArith "log" a
    sqrt a = UnaryArith "sqrt" a
    a ** b = BinaryArith Pow a b
    sin a = UnaryArith "sin" a
    cos a = UnaryArith "cos" a
    tan a = UnaryArith "tan" a
    asin a = UnaryArith "asin" a
    acos a = UnaryArith "acos" a
    atan a = UnaryArith "atan" a
    sinh a = UnaryArith "sinh" a
    cosh a = UnaryArith "cosh" a
    tanh a = UnaryArith "tanh" a
    asinh a = UnaryArith "asinh" a
    acosh a = UnaryArith "acosh" a
    atanh a = UnaryArith "atanh" a

prettyShow :: (Num a, Ord a, Show a) => SymbolicManip a -> String
prettyShow (Number x) = if x >= 0 then show x else "(" ++ show x ++ ")"
prettyShow (Symbol x) = x
prettyShow (BinaryArith op a b) =
    let pa = simpleParen a
        pb = simpleParen b
        pop = op2str op
    in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) = opstr ++ "(" ++ show a ++ ")"

op2str :: Op -> String
op2str Plus = "+"
op2str Minus = "-"
op2str Mul = "*"
op2str Div = "/"
op2str Pow = "**"

simpleParen :: (Show a, Num a, Ord a) => SymbolicManip a -> String
simpleParen x@(Number _) = show x
simpleParen x@(Symbol _) = show x
simpleParen x@(BinaryArith _ _ _) = "(" ++ show x ++ ")"
simpleParen x@(UnaryArith _ _) = "(" ++ show x ++ ")"

instance (Show a, Num a, Ord a) => Show (SymbolicManip a) where show a = prettyShow a

rpnShow :: Show a => SymbolicManip a -> String
rpnShow i =
    let toList (Number x) = [show x]
        toList (Symbol x) = [x]
        toList (BinaryArith op a b) = toList a ++ toList b ++ [op2str op]
        toList (UnaryArith op a) = toList a ++ [op]
        join :: [a] -> [[a]] -> [a]
        join delim l = concat (intersperse delim l)
    in join " " (toList i)

simplify :: (Eq a, Num a) => SymbolicManip a -> SymbolicManip a
simplify (BinaryArith op ia ib) =
    let sa = simplify ia
        sb = simplify ib
    in case (op, sa, sb) of
         (Mul, Number 1, n) -> n
         (Mul, n, Number 1) -> n
         (Mul, Number (-1), Number n) -> Number (-n)
         (Mul, Number n, Number (-1)) -> Number (-n)
         (Mul, Number 0, _) -> Number 0
         (Mul, _, Number 0) -> Number 0
         (Div, n, Number 1) -> n
         (Div, Number n, Number n') | n == n' -> Number 1
         (Plus, Number 0, n) -> n
         (Plus, n, Number 0) -> n
         (Minus, n, Number 0) -> n
         (Minus, Number 0, Number n) -> Number (-n)
         _ -> BinaryArith op sa sb
simplify (UnaryArith op a) = UnaryArith op (simplify a)
simplify x = x

