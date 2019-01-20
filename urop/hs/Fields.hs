{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fields (C, Field (..), Fp, Prime (..), Q, R, Val (..), modInv) where

import Data.Complex (Complex (..))
import Data.Ratio (denominator, numerator)

------------------------------------------------------------------------------
-- Fields

-- Value information
data Val = None | NumDen Integer Integer | ReIm Double Double

-- Field type class
class (Eq f, Fractional f, Show f) => Field f where
  {-# MINIMAL char, val #-}
  char :: f -> Integer
  val :: f -> Val

------------------------------------------------------------------------------
-- Characteristic zero

-- Rational numbers Q
newtype Q = Q Rational deriving (Eq, Fractional, Num, Ord)
instance Show Q where
  show q = show n ++ if d == 1 then "" else "/" ++ show d
    where
      NumDen n d = val q
instance Field Q where
  char = const 0
  val (Q q) = NumDen (numerator q) (denominator q)

-- Real numbers R
newtype R = R Double deriving (Eq, Fractional, Num, Ord)
instance Show R where
  show (R r) = if r == fromIntegral r' then show r' else show r
    where
      r' = floor r
instance Field R where
  char = const 0
  val = const None

-- Complex numbers C
newtype C = C (Complex Double) deriving (Eq, Fractional, Num)
instance Show C where
  show (C (r :+ i))
    | signum i == -1 = "(" ++ show (R r) ++ "-" ++ show (R (abs i)) ++ "i)"
    | signum i == 1 = "(" ++ show (R r) ++ "+" ++ show (R i) ++ "i)"
    | otherwise = show (R r)
instance Field C where
  char = const 0
  val (C (r :+ i)) = ReIm r i

------------------------------------------------------------------------------
-- Characteristic prime

-- Prime numbers
class (Enum p, Show p) => Prime p where
  prime :: p -> Integer
  prime _ = read . tail . show $ (toEnum 0 :: p)

-- Prime subfields Fp
newtype Fp p = Fp Integer

-- Fp field instance
instance Prime p => Field (Fp p) where
  char = const $ prime (undefined :: p)
  val = const None

-- Fp standard instances
instance Prime p => Bounded (Fp p) where
  minBound = 0
  maxBound = -1
instance Prime p => Enum (Fp p) where
  toEnum = fromIntegral
  fromEnum (Fp n) = fromInteger n
instance Eq (Fp p) where
  Fp n == Fp n' = fromInteger n == fromInteger n'
instance Prime p => Fractional (Fp p) where
  fromRational n = fromInteger (numerator n) / fromInteger (denominator n)
  recip (Fp n) = case modInv n $ prime (undefined :: p) of
    Right m -> fromIntegral m
    Left m -> error $ show m
instance Prime p => Integral (Fp p) where
  quotRem (Fp n) (Fp n') = (fromInteger q, fromInteger r)
    where
      (q, r) = quotRem n n'
  toInteger (Fp n) = fromInteger n
instance Prime p => Num (Fp p) where
  Fp n + Fp n' = fromInteger $ n + n'
  Fp n * Fp n' = fromInteger $ n * n'
  abs n = n
  signum n = if n == 0 then 0 else 1
  fromInteger n = Fp . mod n $ prime (undefined :: p)
  negate (Fp n) = fromInteger $ (-n)
instance Ord (Fp p) where
  Fp n <= Fp n' = n <= n'
instance Prime p => Real (Fp p) where
  toRational (Fp n) = fromInteger n
instance Show (Fp p) where
  show (Fp n) = show n

-- Extended Euclidean algorithm
extGCD :: Integral a => a -> a -> ((a, a), a)
extGCD 0 y = ((0, 1), y)
extGCD x y = ((t - s * q, s), g)
  where
    (q, r) = quotRem y x
    ((s, t), g) = extGCD r x

-- Modular inverse
modInv :: Integral a => a -> a -> Either a a
modInv x p = if g == 1 then return (mod y p) else Left g
  where
    ((y, _), g) = extGCD x p