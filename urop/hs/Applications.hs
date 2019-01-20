{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Applications (Divisor (..), Key (..), Keys (..), division, dh, ecdh,
  fermat, lenstra, modExp, pocklington, pollard, rsa, trial) where

import Fields
import WeierstrassEquations
import GroupLaw

import Control.Exception (SomeException, evaluate, try)
import System.IO.Unsafe (unsafePerformIO)

------------------------------------------------------------------------------
-- Auxiliary functions

-- Probable primes
primes :: [Integer]
primes = 2 : 3 : concatMap ((<*>) [pred, succ] . return) [6, 12 ..]

-- Modular exponentiation
modExp :: (Integral a, Integral b) => a -> b -> a -> a
modExp _ 0 _ = 1
modExp 0 _ _ = 0
modExp x e p
  | odd e = mod (x * m) p
  | otherwise = m
  where
    m = modExp (mod (x * x) p) (quot e 2) p

------------------------------------------------------------------------------
-- Integer factorisation

-- Integer factorisation divisor data
newtype Divisor = Divisor (Either String Integer)
instance Show Divisor where
  show (Divisor (Left s)) = s
  show (Divisor (Right n)) = show n ++ " is a divisor"
 
-- Naive integer factorisation
division :: Integer -> [Integer]
division = division' primes
  where
    division' ts' @ (t : ts) n
      | n <= 1 = []
      | r == 0 = t : division' ts' q
      | otherwise = division' ts n
      where
        (q, r) = quotRem n t

-- Pollard's p - 1 method with given smoothness bound b
pollard :: Integer -> Integer -> Divisor
pollard n b = Divisor $ pollard' 2
  where
    l = foldr lcm 1 [2 .. b]
    pollard' a = case gcd n . pred $ modExp a l n of
      1 -> Left "Choose a larger smoothness bound"
      g -> if g < n then return g else pollard' (succ a)

-- Lenstra's elliptic curve factorisation method
-- with given (x, y) coordinates and smoothness bound c
lenstra :: Prime p => Fp p -> Fp p -> Fp p -> Integer -> Divisor
lenstra (_ :: Fp p) x y c = Divisor $ lenstra' (succ minBound :: Fp p)
  where
    n = char (undefined :: Fp p)
    l = foldr lcm 1 [2 .. c]
    lenstra' a = case gcd n d of
      1 -> unsafe (mul l p) (lenstra' a')
        where
          e = either error id $ sW a b
          p = A e x y
      g -> if g < n then return g else lenstra' a'
      where
        a' = succ a
        b = y ^ 2 - x ^ 3 - a * x
        d = 4 * fromIntegral a ^ 3 + 27 * fromIntegral b ^ 2

-- Haskell-specific IO hack, not safe for work
unsafe :: Monad m => P (Fp p) -> m Integer -> m Integer
unsafe x y = unsafePerformIO $ try' x >>= return' y
  where
    try' :: P (Fp p) -> IO (Either SomeException (P (Fp p)))
    try' = try . evaluate
    read' = reads :: ReadS Integer
    gcd' = return . return . fst . head . read' . show
    return' = either gcd' . const . return

------------------------------------------------------------------------------
-- Primality testing

-- Primality certificate data
data Certificate = Threshold | Composite Integer | Definite Integer
                 | Certified Integer (Integer, [Integer]) [(Integer, Integer)]
instance Show Certificate where
  show Threshold = "Choose a larger trial division threshold"
  show (Composite n) = show n ++ " is composite"
  show (Definite n) = show n ++ " is definitely prime"
  show (Certified n (d, fs) cs) = "N = " ++ show n
    ++ " is certified prime\nr = " ++ show d ++ " divides N - 1 and r = Pi "
    ++ show fs ++ "\n" ++ concatMap (uncurry show') cs
    where
      show' p a = show a ++ "^(N - 1) = 1 mod N and gcd(" ++ show a
        ++ "^((N - 1)/" ++ show p ++ ") - 1, N) = 1 mod N \n"

-- Naive primality test
trial :: Integer -> Bool
trial n = n > 1 && all indivisible primes'
  where
    squareRoot = floor . sqrt . fromInteger
    primes' = takeWhile (<= squareRoot n) primes
    indivisible = (/= 0) . mod n

-- Fermat's little theorem
fermat :: Integer -> Integer -> Bool
fermat n t = if n < 4 then trial n else
  n > 1 && all one [2 .. mod (t - 1) (n - 3) + 2]
  where
    one p = modExp p (pred n) n == 1

-- Pocklington-Lehmer primality test with a trial division threshold t
pocklington :: Integer -> Integer -> Certificate
pocklington n b
  | n <= b || n <= 2 = if trial n then Definite n else Composite n
  | otherwise = pocklington' 1 primes $ pred n
  where
    pocklington' d ps' @ (p : ps) n'
      | p > b = Threshold
      | d >= floor (sqrt $ fromInteger n) = Certified n (d, []) []
      | r == 0 = case generate 2 of
        Just a -> case pocklington' (p * d) ps' q of
          Certified m (d', fs) cs @ ((p', _) : _)
            | p == p' -> Certified m (d', p : fs) cs
          Certified m (d', fs) cs -> Certified m (d', p : fs) $ (p, a) : cs
          c -> c
        _ -> Composite n
      | otherwise = pocklington' d ps n'
      where
        (q, r) = quotRem n' p
        generate a
          | a >= n = Nothing
          | modExp a' p n == 1 && gcd (pred a') n == 1 = Just a
          | otherwise = generate $ succ a
          where
            a' = modExp a (quot (pred n) p) n

------------------------------------------------------------------------------
-- Cryptography

-- Public and private key data
data Keys = Keys Key Key | Locked
instance Show Keys where
  show (Keys e d) = "Public encryption key is " ++ show e
    ++ "\nPrivate decryption key is " ++ show d
  show _ = "One of p or q is not prime"

-- Encryption or decryption key data
data Key = Key Integer Integer
instance Show Key where
  show (Key n k) = "(" ++ show n ++ ", " ++ show k ++ ")"

-- Rivest-Shamir-Adleman cryptosystem with primes p and q
rsa :: Integer -> Integer -> Keys
rsa p q = case modInv e l of
  Right d -> Keys (Key n e) (Key n d)
  _ -> Locked
  where
    n = p * q
    l = lcm (pred p) (pred q)
    e = head $ filter ((== 1) . gcd l) [2 ..]

-- Key exchange data
data Exchange a where
  Exchange :: Show a => a -> a -> a -> Exchange a
instance Show (Exchange a) where
  show (Exchange a b s) = "First key is " ++ show a ++ "\nSecond key is "
    ++ show b ++ "\nPrivate symmetric key is " ++ show s

-- Diffie-Hellman key exchange
dh :: Key -> Integer -> Integer -> Exchange Integer
dh (Key p g) a b = Exchange (modExp g a p) (modExp g b p) (modExp g (a * b) p)

-- Elliptic curve Diffie-Hellman
ecdh :: Prime p => P (Fp p) -> Integer -> Integer -> Exchange (P (Fp p))
ecdh p a b = Exchange (mul a p) (mul b p) (mul (a * b) p)