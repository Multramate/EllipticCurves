{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module WeierstrassEquations (EC (..), discriminant, isSmooth, jInvariant,
  isIsomorphic, lW, l2m, mW, m2s, sW) where

import Fields

------------------------------------------------------------------------------
-- Elliptic curves

-- Weierstrass curve data
data EC f where
  EC :: Field f => f -> f -> f -> f -> f -> EC f
deriving instance Eq (EC f)
instance Show (EC f) where
  show (EC a_1 a_2 a_3 a_4 a_6) = "y^2" ++ show' a_1 "xy" ++ show' a_3 "y"
    ++ " = " ++ "x^3" ++ show' a_2 "x^2" ++ show' a_4 "x" ++ show' a_6 ""
    where
      show' n s
        | signum n == 1 = " + " ++ show n ++ s
        | signum n == -1 = " - " ++ show (abs n) ++ s
        | otherwise = ""

-- Quantities
a_1, a_2, a_3, a_4, a_6, b_2, b_4, b_6, b_8, c_4, c_6 :: EC f -> f
a_1 (EC a_1 _ _ _ _) = a_1
a_2 (EC _ a_2 _ _ _) = a_2
a_3 (EC _ _ a_3 _ _) = a_3
a_4 (EC _ _ _ a_4 _) = a_4
a_6 (EC _ _ _ _ a_6) = a_6
b_2 e @ (EC _ _ _ _ _) = a_1 e ^ 2 + 4 * a_2 e
b_4 e @ (EC _ _ _ _ _) = a_1 e * a_3 e + 2 * a_4 e
b_6 e @ (EC _ _ _ _ _) = a_3 e ^ 2 + 4 * a_6 e
b_8 e @ (EC _ _ _ _ _) = a_1 e ^ 2 * a_6 e + 4 * a_2 e * a_6 e
  - a_1 e * a_3 e * a_4 e + a_2 e * a_3 e ^ 2 - a_4 e ^ 2
c_4 e @ (EC _ _ _ _ _) = b_2 e ^ 2 - 24 * b_4 e
c_6 e @ (EC _ _ _ _ _) = 36 * b_2 e * b_4 e - b_2 e ^ 3 - 216 * b_6 e

------------------------------------------------------------------------------
-- Smoothness and isomorphism

-- Discriminant
discriminant :: EC f -> f
discriminant e @ (EC _ _ _ _ _) = 9 * b_2 e * b_4 e * b_6 e
  - b_2 e ^ 2 * b_8 e - 8 * b_4 e ^ 3 - 27 * b_6 e ^ 2

-- Check if elliptic curve is smooth by discriminant
isSmooth :: EC f -> Bool
isSmooth e @ (EC _ _ _ _ _) = discriminant e /= 0

-- Verify that elliptic curve is smooth by discriminant
assertSmooth :: EC f -> Either String (EC f)
assertSmooth e = if isSmooth e then return e else
  Left $ "Curve " ++ show e ++ " is not smooth"

-- J-invariant
jInvariant :: EC f -> f
jInvariant e @ (EC _ _ _ _ _) = c_4 e ^ 3 / discriminant e

-- Check if elliptic curves are isomorphic by j-invariant
isIsomorphic :: EC f -> EC f -> Bool
isIsomorphic e @ (EC _ _ _ _ _) e' @ (EC _ _ _ _ _) =
  jInvariant e == jInvariant e'

-- Verify that elliptic curves are isomorphic by j-invariant
assertIsomorphic :: EC f -> EC f -> Either String (EC f)
assertIsomorphic e e' = if isIsomorphic e e' then return e' else
  Left $ "Curves " ++ show e ++ " and " ++ show e' ++ " are not isomorphic"

------------------------------------------------------------------------------
-- Affine transformations

-- Elliptic curve from long Weierstrass equation
lW :: Field f => f -> f -> f -> f -> f -> Either String (EC f)
lW = ((((assertSmooth .) .) .) .) . EC

-- Convert long to medium Weierstrass equation
l2m :: Field f => EC f -> Either String (EC f)
l2m e = mW (b_2 e / 4) (b_4 e / 2) (b_6 e / 4) >>= assertIsomorphic e

-- Elliptic curve from medium Weierstrass equation
mW :: Field f => f -> f -> f -> Either String (EC f)
mW = ((assertSmooth .) .) . flip (EC 0) 0

-- Convert medium to short Weierstrass equation
m2s :: Field f => EC f -> Either String (EC f)
m2s e = sW (-c_4 e / 48) (-c_6 e / 864) >>= assertIsomorphic e

-- Elliptic curve from short Weierstrass equation
sW :: Field f => f -> f -> Either String (EC f)
sW = (assertSmooth .) . EC 0 0 0