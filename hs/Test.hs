{-# LANGUAGE ScopedTypeVariables #-}

module Test where

import Fields
import WeierstrassEquations
import GroupLaw
import Rationals
import Applications

import Data.Either (rights)

------------------------------------------------------------------------------
-- Prime and composite integers

data P2 = P2 deriving (Enum, Show)
instance Prime P2

data P3 = P3 deriving (Enum, Show)
instance Prime P3

data P5 = P5 deriving (Enum, Show)
instance Prime P5

data P199843247 = P199843247 deriving (Enum, Show)
instance Prime P199843247

data P1715761513 = P1715761513 deriving (Enum, Show)
instance Prime P1715761513

data P2147483647 = P2147483647 deriving (Enum, Show)
instance Prime P2147483647

------------------------------------------------------------------------------
-- Auxiliary functions

throw :: Either String a -> a
throw = either error id

writeLMS :: Field f => EC f -> String
writeLMS e = "Weierstrass equation transformations:\nfrom long " ++ show e
  ++ ",\nto medium " ++ show (throw $ l2m e)
  ++ ",\nand to short " ++ show (throw $ l2m e >>= m2s) ++ "\n"

writeAddition :: Field f => P f -> P f -> String
writeAddition p p' = "Point P = " ++ show p ++ " and P' = " ++ show p'
  ++ " addition:\nP + P' = " ++ show (add p p') ++ "\n"

writeFinite :: Prime p => EC (Fp p) -> String
writeFinite (e :: EC (Fp p)) = "Group of " ++ show e ++ " over F_"
  ++ show (char (undefined :: Fp p)) ++ ":\n"
  ++ unlines (map show $ enumPoints e)

writeTorsion :: EC Q -> String
writeTorsion e = "Torsion subgroup of " ++ show e ++ ":\n"
  ++ unlines (map show $ computeTors e)

writeRank :: EC Q -> String
writeRank e = "Rank equations for a of " ++ show e ++ ":\n"
  ++ unlines (map show a) ++ "Rank equations for a' of " ++ show e ++ ":\n"
  ++ unlines (map show a')
  where
    (a, a') = getRankEqns e

writeDivision :: Integer -> String
writeDivision n = "Naive integer factorisation on " ++ show n ++ ":\n"
  ++ show n ++ " = Pi " ++ show (division n) ++ "\n"

writePollard :: Integer -> Integer -> String
writePollard n b = "Pollard's p - 1 method on " ++ show n ++ " with B = "
  ++ show b ++ ":\n" ++ show (pollard n b) ++ "\n"

writeLenstra :: Prime p => Fp p -> Fp p -> Fp p -> Integer -> String
writeLenstra (f :: Fp p) x y c =
  "Lenstra's elliptic curve factorisation method on "
  ++ show (char (undefined :: Fp p)) ++ " with C = " ++ show c ++ ":\n"
  ++ show (lenstra f x y c) ++ "\n"

writeTrial :: Integer -> String
writeTrial n = "Naive primality test on " ++ show n ++ ":\n" ++ show n
  ++ (if trial n then " is prime" else " is composite") ++ "\n"

writeFermat :: Integer -> Integer -> String
writeFermat n t = "Fermat's primality test on " ++ show n ++ " with "
  ++ show t ++ " tests:\n" ++ show n
  ++ (if fermat n t then " is probably prime" else " is composite") ++ "\n"

writePocklington :: Integer -> Integer -> String
writePocklington n b = "Pocklington-Lehmer primality test on " ++ show n
  ++ " with B = " ++ show b ++ ":\n" ++ show (pocklington n b)

writeRSA :: Integer -> Integer -> Integer -> String
writeRSA p q m = "Rivest-Shamir-Adleman cryptosystem on p = " ++ show p
  ++ " and q = " ++ show q ++ ":\n" ++ show (rsa p q) ++ "\n"

writeDH :: Key -> Integer -> Integer -> String
writeDH k @ (Key p g) a b =
  "Diffie-Hellman key exchange on p = " ++ show p ++ " and g = " ++ show g
  ++ ":\n" ++ show (dh k a b) ++ "\n"

writeECDH :: Prime p => P (Fp p) -> Integer -> Integer -> String
writeECDH (p @ (A e x y) :: P (Fp p)) a b =
  "Elliptic curve Diffie-Hellman on P = " ++ show p ++ ":\nP in E : "
  ++ show e ++ " over F_" ++ show (char (undefined :: Fp p)) ++ "\n"
  ++ show (ecdh p a b) ++ "\n"
writeECDH _ _ _ =
  "Elliptic curve Diffie-Hellman on O:\nChoose a different point\n"

------------------------------------------------------------------------------
-- Main functions

main :: IO ()
main = writeFile "Output.txt" . unlines $
  [ writeLMS eLMS
  , writeAddition (A eAddition 0 (-1)) (A eAddition 1 2)
  , writeAddition (A eAddition 0 (-1)) (A eAddition 0 (-1))
  , writeAddition (A eAddition 1 2) (A eAddition 1 2)
  ] ++
  map writeFinite e2s ++
  map writeFinite e3s ++
  map writeFinite [e5] ++
  [ writeTorsion e0
  , writeTorsion e0'
  , writeTorsion e1
  , writeTorsion e2
  , writeTorsion e3
  , writeTorsion e4
  , writeRank e1
  , writeRank e2
  , writeRank e3
  , writeRank e4
  , writeDivision 420
  , writeDivision 421
  , writePollard 246082373 7
  , writePollard 246082373 9
  , writePollard 7591548931 20
  , writePollard 7591548931 25
  , writeLenstra (undefined :: Fp P1715761513) 2 1 17
  , writeLenstra (undefined :: Fp P199843247) 1 1 11
  , writeTrial 420
  , writeTrial 421
  , writeTrial 561
  , writeFermat 420 1
  , writeFermat 421 1
  , writeFermat 561 1
  , writeFermat 561 2
  , writePocklington 2147483647 200
  , writePocklington 9223372036854775783 400000
  , writeRSA 2147483647 2147483659 123456789
  , writeDH (Key 2147483647 65537) 16777259 16777289
  , writeECDH (A eDH 0 1) 16777259 16777289
  ]
  where
    eLMS = throw $ lW 2 (-1) (1 / 3) (-1 / 3) (-1 / 27) :: EC Q
    eAddition = throw $ sW 2 1 :: EC Q
    e0 = throw $ sW 0 4 :: EC Q
    e0' = throw $ sW 0 (-4) :: EC Q
    e1 = throw $ sW (-1) 0 :: EC Q
    e2 = throw $ sW (-5) 0 :: EC Q
    e3 = throw $ sW (-17) 0 :: EC Q
    e4 = throw $ sW (-226) 0 :: EC Q
    e5 = throw $ sW 1 1 :: EC (Fp P5)
    e2s = rights [lW a_1 a_2 a_3 a_4 a_5 | a_1 <- values, a_2 <- values,
      a_3 <- values, a_4 <- values, a_5 <- values] :: [EC (Fp P2)]
      where
        values = [minBound .. maxBound]
    e3s = rights [mW a b c | a <- values, b <- values, c <- values]
      :: [EC (Fp P3)]
      where
        values = [minBound .. maxBound]
    eDH = throw $ sW 65537 1 :: EC (Fp P2147483647)