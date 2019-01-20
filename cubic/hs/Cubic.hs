{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Data.Ratio (denominator, numerator)

------------------------------------------------------------------------------
-- Rational numbers

newtype Q = Q Rational
          deriving (Enum, Eq, Fractional, Num, Ord, Real, RealFrac)
instance Show Q where
  show q
    | d == 1    = show n
    | otherwise = show n ++ "/" ++ show d
    where
      n = num q
      d = den q

num :: Q -> Integer
num (Q q) = numerator q

den :: Q -> Integer
den (Q q) = denominator q

------------------------------------------------------------------------------
-- Algebraic curves

data L = L Q Q Q
       deriving Eq
instance Show L where
  show (L a b c) = show a ++ "X + " ++ show b ++ "Y + " ++ show c ++ "Z = 0"

data C = C Q Q Q Q Q Q Q Q Q Q
       deriving Eq
instance Show C where
  show (C a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
    =  show a0 ++  "X^3 + " ++ show a1 ++ "X^2Y + "
    ++ show a2 ++ "XY^2 + " ++ show a3 ++  "Y^3 + "
    ++ show a4 ++ "X^2Z + " ++ show a5 ++  "XYZ + "
    ++ show a6 ++ "Y^2Z + " ++ show a7 ++ "XZ^2 + "
    ++ show a8 ++ "YZ^2 + " ++ show a9 ++  "Z^3 = 0"

data CP = P C Q Q Q
instance Eq CP where
  P c x y z == P c' x' y' z' = c == c'
    && ((x /= 0 && x' /= 0 && (y/x, z/x) == (y'/x', z'/x'))
    ||  (y /= 0 && y' /= 0 && (x/y, z/y) == (x'/y', z'/y'))
    ||  (z /= 0 && z' /= 0 && (x/z, y/z) == (x'/z', y'/z')))
instance Show CP where
  show (P _ x y z) = show [x, y, z]

evaluate :: CP -> Q
evaluate (P (C a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) x y z)
  = a0*x^3 + a1*x^2*y + a2*x*y^2 + a3*y^3
  + a4*x^2*z + a5*x*y*z + a6*y^2*z + a7*x*z^2 + a8*y*z^2 + a9*z^3

tangent :: CP -> L
tangent (P (C a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) x y z) = L dx dy dz
  where
    dx = 3*a0*x^2 + 2*a1*x*y + a2*y^2 + 2*a4*x*z + a5*y*z + a7*z^2
    dy = a1*x^2 + 2*a2*x*y + 3*a3*y^2 + a5*x*z + 2*a6*y*z + a8*z^2
    dz = a4*x^2 + a5*x*y + a6*y^2 + 2*a7*x*z + 2*a8*y*z + 3*a9*z^2

------------------------------------------------------------------------------
-- Weierstrass equations

data E = E Q Q Q Q Q
       deriving Eq
instance Show E where
  show (E a1 a2 a3 a4 a6)
    =  "y^2" ++ show' a1  "xy" ++ show' a3 "y" ++ " = "
    ++ "x^3" ++ show' a2 "x^2" ++ show' a4 "x" ++ show' a6 ""
    where
      show' n s
        | signum n ==  1 = " + " ++ show      n  ++ s
        | signum n == -1 = " - " ++ show (abs n) ++ s
        | otherwise      = ""

data EP = A E Q Q
        | O
        deriving Eq
instance Show EP where
  show (A _ x y) = show (x, y)
  show _         = "O"

------------------------------------------------------------------------------
-- Linear algebra

type M = [[Q]]

identity :: M
identity = [[1, 0, 0], [0, 1, 0], [0, 0, 1]]

minor :: Int -> Int -> M -> M
minor r c = delete r . map (delete c)
  where
    delete n = uncurry (++) . fmap tail . splitAt n

determinant :: M -> Q
determinant [[q]] = q
determinant m     = sum (alternate (zipWith (*) (head m) minors))
  where
    minors    = map (determinant . flip (minor 0) m) [0 .. pred (length m)]
    alternate = zipWith ($) (cycle [id, negate])

transpose :: M -> M
transpose ([] : _) = []
transpose m        = map head m : transpose (map tail m)

dot :: [Q] -> [Q] -> Q
dot = (.) sum . zipWith (*)

compose :: M -> M -> M
compose m m' = map (flip map (transpose m') . dot) m

invert3 :: M -> M
invert3 m = map (map (/ determinant m)) [[ det 0 0, -det 0 1,  det 0 2],
                                         [-det 1 0,  det 1 1, -det 1 2],
                                         [ det 2 0, -det 2 1,  det 2 2]]
  where
    det r c = determinant (minor c r m)

------------------------------------------------------------------------------
-- Point choices

choosePoint :: CP -> CP
choosePoint p @ (P c x y z) = points !! 5 -- choice
  where
    L dx dy dz = tangent p
    range n    = [pred n .. succ n]
    xs         = [P c (-dy*y' - dz*z') (dx*y') (dx*z')
                 | y' <- range y, z' <- range z, (y', z') /= (0, 0)]
    ys         = [P c (dy*x') (-dx*x' - dz*z') (dy*z')
                 | x' <- range x, z' <- range z, (x', z') /= (0, 0)]
    zs         = [P c (dz*x') (dz*y') (-dx*x' - dy*y')
                 | x' <- range x, y' <- range y, (x', y') /= (0, 0)]
    points     = filter ((/= 0) . evaluate) (xs ++ ys ++ zs)

chooseInvertible :: CP -> M
chooseInvertible p @ (P _ x y z) = invertible !! 0 -- choice
  where
    P _ x' y' z' = choosePoint p
    matrices     = map (([x', y', z'] :) . ([x, y, z] :) . (: [])) identity
    invertible   = [transpose m | m <- matrices, determinant m /= 0]

------------------------------------------------------------------------------
-- Projective transformations

data Trans c = Trans M c
instance Show c => Show (Trans c) where
  show (Trans m @ [[m11, m12, m13],
                   [m21, m22, m23],
                   [m31, m32, m33]] c)
    =  "X  = " ++ show'' m11  "X'" m12  "Y'" m13  "Z'" ++ "\n"
    ++ "Y  = " ++ show'' m21  "X'" m22  "Y'" m23  "Z'" ++ "\n"
    ++ "Z  = " ++ show'' m31  "X'" m32  "Y'" m33  "Z'" ++ "\n"
    ++ "X' = " ++ show'' m11' "X"  m12' "Y"  m13' "Z"  ++ "\n"
    ++ "Y' = " ++ show'' m21' "X"  m22' "Y"  m23' "Z"  ++ "\n"
    ++ "Z' = " ++ show'' m31' "X"  m32' "Y"  m33' "Z"  ++ "\n"
    ++ show c
    where
      [[m11', m12', m13'],
       [m21', m22', m23'],
       [m31', m32', m33']]  = invert3 m
      show' n s
        | signum n ==  1    = " + " ++ show      n  ++ s
        | signum n == -1    = " - " ++ show (abs n) ++ s
        | otherwise         = ""
      show''  0 _  0 _ m3 z = show m3 ++ z
      show''  0 _ m2 y m3 z = show m2 ++ y ++ show' m3 z
      show'' m1 x m2 y m3 z = show m1 ++ x ++ show' m2 y ++ show' m3 z

data Transformed c = Transformed {trans :: M -> Trans c}
instance Functor Transformed where
  fmap f c = Transformed $ \m -> let Trans m' c' = trans c m
                                  in Trans m' (f c')
instance Applicative Transformed where
  pure = Transformed . flip Trans
  f <*> c = Transformed $ \m -> let Trans m' f'  = trans f m
                                    Trans m'' c' = trans c m'
                                 in Trans m'' (f' c')
instance Monad Transformed where
  return = pure
  c >>= f = Transformed $ \m -> let Trans m' c' = trans c m
                                 in trans (f c') m'
instance Show c => Show (Transformed c) where
  show = show . flip trans identity

transform :: M -> C -> Transformed C
transform m @ [[m11, m12, m13],
               [m21, m22, m23],
               [m31, m32, m33]] (C a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
  = Transformed $ \m' ->
    Trans (compose m' m) (C a0' a1' a2' a3' a4' a5' a6' a7' a8' a9')
  where
    a0' = a0*m11^3 + a1*m11^2*m21 + a2*m11*m21^2 + a3*m21^3 + a4*m11^2*m31
        + a5*m11*m21*m31 + a6*m21^2*m31 + a7*m11*m31^2 + a8*m21*m31^2 + a9*m31^3
    a1' = a0*3*m11^2*m12 + a1*(m11^2*m22 + 2*m11*m12*m21)
        + a2*(2*m11*m21*m22 + m12*m21^2) + a3*3*m21^2*m22
        + a4*(m11^2*m32 + 2*m11*m12*m31)
        + a5*(m11*m21*m32 + m11*m22*m31 + m12*m21*m31)
        + a6*(m21^2*m32 + 2*m21*m22*m31) + a7*(2*m11*m31*m32 + m12*m31^2)
        + a8*(2*m21*m31*m32 + m22*m31^2) + a9*3*m31^2*m32
    a2' = a0*3*m11*m12^2 + a1*(2*m11*m12*m22 + m12^2*m21)
        + a2*(m11*m22^2 + 2*m12*m21*m22) + a3*3*m21*m22^2
        + a4*(2*m11*m12*m32 + m12^2*m31)
        + a5*(m11*m22*m32 + m12*m21*m32 + m12*m22*m31)
        + a6*(2*m21*m22*m32 + m22^2*m31) + a7*(m11*m32^2 + 2*m12*m31*m32)
        + a8*(m21*m32^2 + 2*m22*m31*m32) + a9*3*m31*m32^2
    a3' = a0*m12^3 + a1*m12^2*m22 + a2*m12*m22^2 + a3*m22^3 + a4*m12^2*m32
        + a5*m12*m22*m32 + a6*m22^2*m32 + a7*m12*m32^2 + a8*m22*m32^2 + a9*m32^3
    a4' = a0*3*m11^2*m13 + a1*(m11^2*m23 + 2*m11*m13*m21)
        + a2*(2*m11*m21*m23 + m13*m21^2) + a3*3*m21^2*m23
        + a4*(m11^2*m33 + 2*m11*m13*m31)
        + a5*(m11*m21*m33 + m11*m23*m31 + m13*m21*m31)
        + a6*(m21^2*m33 + 2*m21*m23*m31) + a7*(2*m11*m31*m33 + m13*m31^2)
        + a8*(2*m21*m31*m33 + m23*m31^2) + a9*3*m31^2*m33
    a5' = a0*6*m11*m12*m13 + a1*(2*m11*m12*m23 + 2*m11*m13*m22 + 2*m12*m13*m21)
        + a2*(2*m11*m22*m23 + 2*m12*m21*m23 + 2*m13*m21*m22) + a3*6*m21*m22*m23
        + a4*(2*m11*m12*m33 + 2*m11*m13*m32 + 2*m12*m13*m31)
        + a5*(m11*m22*m33 + m11*m23*m32 + m12*m21*m33
            + m12*m23*m31 + m13*m21*m32 + m13*m22*m31)
        + a6*(2*m21*m22*m33 + 2*m21*m23*m32 + 2*m22*m23*m31)
        + a7*(2*m11*m32*m33 + 2*m12*m31*m33 + 2*m13*m31*m32)
        + a8*(2*m21*m32*m33 + 2*m22*m31*m33 + 2*m23*m31*m32) + a9*6*m31*m32*m33
    a6' = a0*3*m12^2*m13 + a1*(m12^2*m23 + 2*m12*m13*m22)
        + a2*(2*m12*m22*m23 + m13*m22^2) + a3*3*m22^2*m23
        + a4*(m12^2*m33 + 2*m12*m13*m32)
        + a5*(m12*m22*m33 + m12*m23*m32 + m13*m22*m32)
        + a6*(m22^2*m33 + 2*m22*m23*m32) + a7*(2*m12*m32*m33 + m13*m32^2)
        + a8*(2*m22*m32*m33 + m23*m32^2) + a9*3*m32^2*m33
    a7' = a0*3*m11*m13^2 + a1*(2*m11*m13*m23 + m13^2*m21)
        + a2*(m11*m23^2 + 2*m13*m21*m23) + a3*3*m21*m23^2
        + a4*(2*m11*m13*m33 + m13^2*m31)
        + a5*(m11*m23*m33 + m13*m21*m33 + m13*m23*m31)
        + a6*(2*m21*m23*m33 + m23^2*m31) + a7*(m11*m33^2 + 2*m13*m31*m33)
        + a8*(m21*m33^2 + 2*m23*m31*m33) + a9*3*m31*m33^2
    a8' = a0*3*m12*m13^2 + a1*(2*m12*m13*m23 + m13^2*m22)
        + a2*(m12*m23^2 + 2*m13*m22*m23) + a3*3*m22*m23^2
        + a4*(2*m12*m13*m33 + m13^2*m32)
        + a5*(m12*m23*m33 + m13*m22*m33 + m13*m23*m32)
        + a6*(2*m22*m23*m33 + m23^2*m32) + a7*(m12*m33^2 + 2*m13*m32*m33)
        + a8*(m22*m33^2 + 2*m23*m32*m33) + a9*3*m32*m33^2
    a9' = a0*m13^3 + a1*m13^2*m23 + a2*m13*m23^2 + a3*m23^3 + a4*m13^2*m33
        + a5*m13*m23*m33 + a6*m23^2*m33 + a7*m13*m33^2 + a8*m23*m33^2 + a9*m33^3

------------------------------------------------------------------------------
-- Weierstrass transformations

project :: CP -> Transformed C
project p @ (P c _ _ _) = transform (chooseInvertible p) c

scaleX :: C -> Transformed C
scaleX (C a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
  | a0 == 0   = error "coefficient of X^3 is zero"
  | otherwise = transform identity $
    C 1 (a1/a0) (a2/a0) (a3/a0) (a4/a0) (a5/a0) (a6/a0) (a7/a0) (a8/a0) (a9/a0)

scaleY :: C -> Transformed C
scaleY c @ (C a0 _ _ _ _ _ a6 _ _ _)
  | a6 == 0   = error "coefficient of Y^2Z is zero"
  | otherwise = transform [[1, 0, 0], [0, 1, 0], [0, 0, -a0/a6]] c

weierstrass :: CP -> Transformed E
weierstrass p = project p >>= scaleX >>= scaleY >>= rearrange
  where
    rearrange (C a0 a1 a2 a3 a4 a5 a6 a7 a8 a9)
      | a0 /= 1   = error "scale X first"
      | a1 /= 0   = error "coefficient of X^2Y is nonzero"
      | a2 /= 0   = error "coefficient of XY^2 is nonzero"
      | a3 /= 0   = error "coefficient of Y^3 is nonzero"
      | a6 /= -1  = error "scale Y first"
      | otherwise = Transformed $ \m ->
        Trans m (E (-a5) a4 (-a8) a7 a9)

transform' :: M -> E -> Transformed E
transform' m (E a1 a2 a3 a4 a6) = Transformed $ \m' ->
  Trans (compose m' m'') (E (-a1') a2' (-a3') a4' a6')
  where
    Trans m'' (C 1 0 0 0 a2' a1' (-1) a4' a3' a6')
      = trans (transform m (C 1 0 0 0 a2 (-a1) (-1) a4 (-a3) a6)) identity

------------------------------------------------------------------------------
-- Prime factorisation

pseudoPrimes :: [Integer]
pseudoPrimes = 2 : 3 : concatMap pair [1 ..]
  where
    pair n = [pred (6*n), succ (6*n)]

primeFactors :: Integer -> [(Integer, Integer)]
primeFactors n
  | n < 0      = (-1, 1) : factorise pseudoPrimes id (-n)
  | otherwise  = factorise pseudoPrimes id n
  where
    factorise _ acc 1 = acc []
    factorise ps' @ (p : ps) acc m
      | mod m p == 0  = factorise ps' (acc . accumulate p) (div m p)
      | otherwise     = factorise ps acc m
    accumulate p []   = [(p, 1)]
    accumulate p' ps' @ ((p, m) : ps)
      | p == p'       = (p, succ m) : ps
      | otherwise     = (p', 1) : ps'

minPrime :: Integer -> Integer
minPrime = fst . head . primeFactors

primePowers :: Integer -> Integer -> Integer
primePowers p n = product [q^div m p | (q, m) <- primeFactors n, m >= p]

------------------------------------------------------------------------------
-- Laska's algorithm

b2 :: E -> Q
b2 (E a1 a2  _  _  _) = a1^2 + 4*a2

b4 :: E -> Q
b4 (E a1  _ a3 a4  _) = a1*a3 + 2*a4

b6 :: E -> Q
b6 (E  _  _ a3  _ a6) = a3^2 + 4*a6

c4 :: E -> Integer
c4 e = int (b2 e^2 - 24*b4 e)
  where
    int q
      | d == 1    = num q
      | otherwise = int (q*fromIntegral (minPrime d)^4)
      where
        d = den q

c6 :: E -> Integer
c6 e = int (36*b2 e*b4 e - b2 e^3 - 216*b6 e)
  where
    int q
      | d == 1    = num q
      | otherwise = int (q*fromIntegral (minPrime d)^6)
      where
        d = den q

scale :: E -> [(Q, (Integer, Integer))]
scale e = map xy (divisors ++ reverse (map (div g) divisors))
  where
    g        = gcd (primePowers 4 (c4 e)) (primePowers 6 (c6 e))
    sqrt'    = floor . sqrt . abs . fromIntegral
    divisors = filter ((== 0) . mod g) [1 .. sqrt' g]
    xy u     = (fromIntegral u, (div (c4 e) (u^4), div (c6 e) (u^6)))

triples :: (Integer, Integer) -> [(Q, Q, Q)]
triples (x, y) = [(fromIntegral a1, fromIntegral a2, fromIntegral a3)
                 | a1 <- [0, 1], a2 <- [-1, 0, 1], a3 <- [0, 1],
                   mod (a1^4 - x) 8 == 0, mod (a2^3 + a1^6 + y) 3 == 0,
                   a1 == 0 || cong a2 a3, a1 == 1 || cong' a3]
  where
    cong a2 a3 = mod (a3 - a2 - div (x - 1) 8) 2 == 0
    cong'   a3 = mod y 8 == 0 && mod (a3^2 - div y 8) 4 == 0

curves :: (Q, (Integer, Integer)) -> [(Q, (Q, Q, Q, Q, Q))]
curves (u, (x, y)) = [(u, (a1, a2, a3, a4', a6'))
                     | (a1, a2, a3) <- triples (x, y),
                       let a4' = a4 a1 a2 a3, let a6' = a6 a1 a2 a3]
  where
    a4 a1 a2 a3 = a1^2/48 + a1^2*a2/6 - a1*a3/2 + a2^2/3 - fromIntegral x/48
    a6 a1 a2 a3 = -a1^6/864 - a1^4*a2/72 + a1^3*a3/24 - a1^2*a2^2/18 
                + a1^2*a4 a1 a2 a3/12 + a1*a2*a3/6 - 2*a2^3/27
                + a2*a4 a1 a2 a3/3 - a3^2/4 - fromIntegral y/864

laska :: E -> Transformed E
laska e @ (E a1 a2 a3 a4 a5) = case concatMap curves (scale e) of
  (u, (a1', a2', a3', a4', a6')) : _ ->
    transform' [[u^2, 0, r], [u^2*s, u^3, t], [0, 0, 1]] e
    where
      s = (u*a1' - a1)/2
      r = (u^2*a2' - a2 + s*a1 + s^2)/3
      t = (u^3*a3' - a3 - r*a1)/2
  _                                  -> error "no minimal model"

------------------------------------------------------------------------------
-- Group law

class Group g where
  {-# MINIMAL o, neg, add #-}
  o   :: g
  neg :: g -> g
  add :: g -> g -> g
  dup :: g -> g
  dup x         = add x x
  mul :: Integral n => n -> g -> g
  mul 0 _       = o
  mul n x
    | n < 0     = neg (mul (-n) x)
    | even n    = y
    | otherwise = add x y
    where
      y = dup (mul (div n 2) x)

instance Group EP where
  o                               = O
  neg (A e @ (E a1 _ a3 _ _) x y) = A e x (-y - a1*x - a3)
  neg _                           = o
  add (A e @ (E a1 a2 a3 a4 a6) x y) (A _ x' y')
    | x /= x'                     = A e (x'' l_a) (y'' l_a m_a)
    | y + y' + a1*x' + a3 /= 0    = A e (x'' l_d) (y'' l_d m_d)
    | otherwise                   = o
    where
      l_a     = (y - y')/n_a
      m_a     = (x*y' - x'*y)/n_a
      n_a     = x - x'
      l_d     = (3*x^2 + 2*a2*x + a4 - a1*y)/n_d
      m_d     = (-x^3 + a4*x + 2*a6 - a3*y)/n_d
      n_d     = 2*y + a1*x + a3
      x'' l   = l^2 + a1*l - a2 - x - x'
      y'' l m = -l*(x'' l) - a1*(x'' l) - m - a3
  add O p                         = p
  add p _                         = p

------------------------------------------------------------------------------
-- Solution

cubic :: C
cubic = C 1 (-3) (-3) 1 (-3) (-5) (-3) (-3) (-3) 1

flex :: CP
flex = P cubic 1 (-1) 0

point :: CP
point = P cubic (-11) (-4) 1

convert :: M -> E -> CP -> EP
convert m e (P _ a b c) = A e (x/z) (y/z)
  where
    [x, y, z] = map (dot [a, b, c]) m

revert :: M -> C -> EP -> CP
revert m e (A _ x y) = P e (a_n*b_d/g) (b_n*a_d/g) g
  where
    [a, b, c]  = map (dot [x, y, 1]) m
    (a_n, a_d) = (fromIntegral (num (a/c)), fromIntegral (den (a/c)))
    (b_n, b_d) = (fromIntegral (num (b/c)), fromIntegral (den (b/c)))
    g          = fromIntegral (gcd (num a_d) (num b_d))
revert _ _ _         = error "no solution"

ans :: CP
ans = revert m cubic (mul 9 (convert (invert3 m) e point))
  where
    Trans m e = trans (weierstrass flex >>= laska) identity