{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module GroupLaw (GroupLaw (..), OrdP, P (..), computeOrder, isDefined,
  enumPoints) where

import Fields
import WeierstrassEquations

import Data.Group (Abelian, Group (..))
import Data.Maybe (catMaybes)

------------------------------------------------------------------------------
-- Points

-- Point data
data P f where
  A :: Field f => EC f -> f -> f -> P f
  O :: P f
deriving instance Eq (P f)
instance Show (P f) where
  show O = "O"
  show (A _ x y) = "(" ++ show x ++ "," ++ show y ++ ")"

-- Check if point is defined in elliptic curve
isDefined :: P f -> Bool
isDefined (A (EC a_1 a_2 a_3 a_4 a_6) x y) = y ^ 2 + a_1 * x * y + a_3 * y
  == x ^ 3 + a_2 * x ^ 2 + a_4 * x + a_6
isDefined _ = True

-- Verify that point is defined in elliptic curve
assertDefined :: P f -> P f
assertDefined p = if isDefined p then p else
  error $ "Point " ++ show p ++ " is not in curve"

------------------------------------------------------------------------------
-- Group instances

-- Elliptic curve is a monoid
instance Field f => Monoid (P f) where
  mempty = O
  mappend p p' = case (assertDefined p, assertDefined p') of
    (O, A _ _ _) -> p'
    (A _ _ _, O) -> p
    (A e @ (EC a_1 a_2 a_3 a_4 a_6) x y, A e' x' y')
      | e /= e' -> error "Curves are different"
      | x /= x' -> A e (x'' l_a) (y'' l_a m_a)
      | y + y' + a_1 * x' + a_3 /= 0 -> A e (x'' l_d) (y'' l_d m_d)
      | otherwise -> O
      where
        l_a = (y - y') / n_a
        m_a = (x * y' - x' * y) / n_a
        n_a = x - x'
        l_d = (3 * x ^ 2 + 2 * a_2 * x + a_4 - a_1 * y) / n_d
        m_d = (-x ^ 3 + a_4 * x + 2 * a_6 - a_3 * y) / n_d
        n_d = 2 * y + a_1 * x + a_3
        x'' l = l ^ 2 + a_1 * l - a_2 - x - x'
        y'' l m = - l * x'' l - a_1 * x'' l - m - a_3
    _ -> O

-- Elliptic curve is a group
instance Field f => Group (P f) where
  invert p = case assertDefined p of
    A e @ (EC a_1 _ a_3 _ _) x y -> A e x $ -y - a_1 * x - a_3
    _ -> O

-- Elliptic curve is an abelian group
instance Field f => Abelian (P f)

-- Elliptic curve has a group law
class Abelian p => GroupLaw p where
  o :: p
  o = mempty
  neg :: p -> p
  neg = invert
  add :: p -> p -> p
  add = mappend
  dup :: p -> p
  dup = mconcat . replicate 2
  mul :: Integral n => n -> p -> p
  mul 0 _ = o
  mul n p
    | n < 0 = neg $ mul (-n) p
    | otherwise = (if even n then id else add p) . dup $ mul (quot n 2) p

-- Elliptic curve over field has a group law
instance Field f => GroupLaw (P f)

------------------------------------------------------------------------------
-- Orders

-- Group of points with order data
data OrdP f where
  OrdP :: Field f => P f -> Int -> OrdP f
deriving instance Eq (OrdP f)
instance Show (OrdP f) where
  show (OrdP p n) = "ord(" ++ show p ++ ") = " ++ show n

-- Compute order of point
computeOrder :: Field f => P f -> Maybe (OrdP f)
computeOrder p = orderPoint' p $ OrdP p 1
  where
    orderPoint' O pq = return pq
    orderPoint' p' @ (A _ x y) (OrdP p'' n) = case (val x, val y) of
      (NumDen _ d', NumDen _ d'')
        | d' > 1 || d'' > 1 -> Nothing
      _ -> orderPoint' (add p' p'') $ OrdP p'' (succ n)

-- Enumerate points with naive approach
enumPoints :: Prime p => EC (Fp p) -> [OrdP (Fp p)]
enumPoints e = catMaybes orders
  where
    values = [minBound .. maxBound]
    points = filter isDefined [A e x y | x <- values, y <- values]
    orders = map computeOrder $ o : points