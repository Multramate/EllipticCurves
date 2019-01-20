module Rationals (computeRank, computeTors, getRankEqns) where

import Fields
import WeierstrassEquations
import GroupLaw

import Data.List (nub, union)
import Data.Maybe (catMaybes)

------------------------------------------------------------------------------
-- Auxiliary functions

-- Type synonym
type Z = Integer

-- Throw errors
throw :: Either String a -> a
throw = either error id

------------------------------------------------------------------------------
-- Torsion computation

-- Elliptic curve E : y^2 = x^3 + Ax + B over Z
data ET = ET Z Z

-- Construct elliptic curve over Q with Z coefficients
eQ :: ET -> EC Q
eQ (ET a b) = throw $ sW (fromInteger a) (fromInteger b)

-- Construct elliptic curve E : y^2 = x^3 + Ax + B over Z
eT :: EC Q -> ET
eT e = ET n'' n'''
  where
    EC _ _ _ a b = throw $ l2m e >>= m2s
    (NumDen n d, NumDen n' d') = (val a, val b)
    (a', b') = (n * d ^ 3 * d' ^ 4, n' * d ^ 6 * d' ^ 5)
    EC _ _ _ a'' b'' = throw $ sW (fromInteger a') (fromInteger b') :: EC Q
    (NumDen n'' _, NumDen n''' _) = (val a'', val b'')

-- Get all non-negative y coordinates such that y^2 | Delta
getYs :: ET -> [Z]
getYs (ET a b) = 0 : filter divisible [1 .. squareRoot delta]
  where
    delta = 4 * a ^ 3 + 27 * b ^ 2
    squareRoot = ceiling . sqrt . fromInteger
    divisible = (== 0) . mod delta . (^ 2)

-- Get all points for each non-negative y coordinate
getPoints :: ET -> Z -> [P Q]
getPoints e @ (ET a b) y = filter isDefined $ map project allXs
  where
    bY2 = abs $ b - y ^ 2
    maxX = if bY2 == 0 then abs a else bY2
    positiveXs = filter ((== 0) . mod bY2) [1 .. maxX]
    allXs = 0 : union positiveXs (map negate positiveXs)
    project = flip (A $ eQ e) (fromInteger y) . fromInteger

-- Compute torsion subgroup
computeTors :: EC Q -> [OrdP Q]
computeTors e = catMaybes $ map computeOrder allPoints
  where
    e' = eT e
    positivePoints = concatMap (getPoints e') (getYs e')
    allPoints = o : union positivePoints (map neg positivePoints)

------------------------------------------------------------------------------
-- Rank computation

-- Elliptic curve E : y^2 = x^3 + Ax^2 + Bx over Z
data ER = ER Z Z

-- Diophantine equation data
data D = D Z Z Z
instance Show D where
  show (D a_ b_ beta) = "Y^2 =" ++ show' beta "X^4"
    ++ show' a_ "X^2Z^2" ++ show' (quot b_ beta) "Z^4"
    where
      show' n s
        | n < 0 = " - " ++ show (abs n) ++ s
        | n > 0 = " + " ++ show n ++ s
        | otherwise = ""

-- Construct elliptic curve E : y^2 = x^3 + Ax^2 + Bx over Z
eR :: EC Q -> ER
eR e = case throw (l2m e) of
  EC _ _ a b 0 -> ER n'' n'''
    where
      (NumDen n d, NumDen n' d') = (val a, val b)
      (a', b') = (n * d * d' ^ 2, n' * d ^ 4 * d' ^ 3)
      EC _ _ a'' b'' 0 = throw $
        mW (fromInteger a') (fromInteger b') 0 :: EC Q
      (NumDen n'' _, NumDen n''' _) = (val a'', val b'')
  _ -> error $ "Curve " ++ show (l2m e) ++ " does not contain (0, 0)"

-- Free squares in integer
freeSquares :: Z -> Z
freeSquares = freeSquares' 2
  where
    freeSquares' n m
      | n' > m = m
      | mod m n' == 0 = freeSquares' n $ quot m n'
      | otherwise = freeSquares' (succ n) m
      where
        n' = n^2

-- Get diophantine equations for image
getImageEqns :: ER -> [D]
getImageEqns (ER a b) = map (D (fromInteger a) (fromInteger b)) allBs
  where
    positiveBs = filter ((== 0) . mod (abs b)) [1 .. abs b]
    squarefreeBs = nub $ map freeSquares positiveBs
    allBs = squarefreeBs ++ map negate squarefreeBs

-- Get diophantine equations for both images
getRankEqns :: EC Q -> ([D], [D])
getRankEqns e = (getImageEqns e', getImageEqns e'')
  where
    e' @ (ER a b) = eR e
    e'' = eR . throw $
      mW (fromInteger (-2 * a)) (fromInteger (a ^ 2 - 4 * b)) 0

-- Compute rank with number of solutions to diophantine equations
computeRank :: Z -> Z -> Z
computeRank e e' = floor . logBase 2 $ fromInteger (e * e') / 4