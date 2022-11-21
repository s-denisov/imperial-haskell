module Calculus where

import Data.Maybe

data UnOp = Neg | Sin | Cos | Log
          deriving (Eq, Ord, Show)

data BinOp = Add | Mul | Div
           deriving (Eq, Ord, Show)

data Exp = Val Double | Id String | UnApp UnOp Exp | BinApp BinOp Exp Exp
         deriving (Eq, Ord, Show)

type Env = [(String, Double)]

---------------------------------------------------------------------------
-- Type classes and class instances

class Vars a where
  x, y, z :: a

instance Vars Exp where
  x = Id "x"
  y = Id "y"
  z = Id "z"

instance Vars Double where
  x = 4.3
  y = 9.2
  z = -1.7

instance Num Exp where
  fromInteger = Val . fromInteger
  negate      = UnApp Neg
  (+)         = BinApp Add
  (*)         = BinApp Mul
-- Leave the following two undefined...
  signum      = undefined
  abs         = undefined

instance Fractional Exp where
  fromRational = Val . fromRational
  (/)          = BinApp Div
-- Leave the following one undefined...
  recip        = undefined

instance Floating Exp where
  sin     = UnApp Sin
  cos     = UnApp Cos
  log     = UnApp Log
-- Leave the following fifteen undefined...
  tan     = undefined
  asin    = undefined
  acos    = undefined
  atan    = undefined
  pi      = undefined
  exp     = undefined
  sqrt    = undefined
  (**)    = undefined
  logBase = undefined
  sinh    = undefined
  cosh    = undefined
  tanh    = undefined
  asinh   = undefined
  acosh   = undefined
  atanh   = undefined

---------------------------------------------------------------------------

lookUp :: Eq a => a -> [(a, b)] -> b
lookUp k = fromJust . lookup k

showExp :: Exp -> String
showExp
  = undefined

eval :: Exp -> Env -> Double
eval (Val x) defs = x
eval (Id x) defs = lookUp x defs
eval (UnApp op e) defs =
  (case op of
    Neg -> negate
    Sin -> sin
    Cos -> cos
    Log -> log
  ) (eval e defs)
eval (BinApp op e1 e2) defs =
  (case op of
    Add -> (+)
    Mul -> (*)
    Div -> (/)
  ) (eval e1 defs) (eval e2 defs)

diff :: Exp -> String -> Exp
diff (Id x) str = if x == str then 1 else 0
diff (Val x) str = 0
diff (UnApp Neg e) str = -diff e str
diff (UnApp Sin e) str = UnApp Cos e * diff e str
diff (UnApp Cos e) str = -UnApp Sin e * diff e str
diff (UnApp Log e) str = diff e str / e
diff (BinApp Add x y) str = diff x str + diff y str
diff (BinApp Mul x y) str = x * diff y str + diff x str * y
diff (BinApp Div x y) str = (x * diff y str - diff x str * y) / (x * x)

maclaurin :: Exp -> Double -> Int -> Double
maclaurin f x n = foldl (\as (df, x', fact, _) -> 
    as + eval df [("x", 0)] * x' / fact
  ) 0 $ take n $ iterate (\(df, x', fact, i) -> 
        (diff df "x", x' * x, fact * i, i + 1)
      ) (f, 1, 1, 1)

-- Uses the Newton-Raphson method to approximate the solution to f(x) = 0, 
-- using n iterations
newtonRaphson :: Exp -> Double -> Int -> Double
newtonRaphson f x n = x - eval f [("x", x)] / eval (diff f "x") [("x", x)]

---------------------------------------------------------------------------
-- Test cases...

e1, e2, e3, e4, e5, e6 :: Exp

-- 5*x
e1 = BinApp Mul (Val 5.0) (Id "x")

-- x*x+y-7
e2 = BinApp Add (BinApp Add (BinApp Mul (Id "x") (Id "x")) (Id "y"))
                (UnApp Neg (Val 7.0))

-- x-y^2/(4*x*y-y^2)::Exp
e3 = BinApp Add (Id "x")
            (UnApp Neg (BinApp Div (BinApp Mul (Id "y") (Id "y"))
            (BinApp Add (BinApp Mul (BinApp Mul (Val 4.0) (Id "x")) (Id "y"))
                        (UnApp Neg (BinApp Mul (Id "y") (Id "y"))))))

-- -cos x::Exp
e4 = UnApp Neg (UnApp Cos (Id "x"))

-- sin (1+log(2*x))::Exp
e5 = UnApp Sin (BinApp Add (Val 1.0)
                           (UnApp Log (BinApp Mul (Val 2.0) (Id "x"))))

-- log(3*x^2+2)::Exp
e6 = UnApp Log (BinApp Add (BinApp Mul (Val 3.0) (BinApp Mul (Id "x") (Id "x")))
                           (Val 2.0))
