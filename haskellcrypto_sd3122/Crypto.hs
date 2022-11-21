module Crypto where

import Data.Char

import Prelude hiding (gcd)

{-
The advantage of symmetric encryption schemes like AES is that they are efficient
and we can encrypt data of arbitrary size. The problem is how to share the key.
The flaw of the RSA is that it is slow and we can only encrypt data of size lower
than the RSA modulus n, usually around 1024 bits (64 bits for this exercise!).

We usually encrypt messages with a private encryption scheme like AES-256 with
a symmetric key k. The key k of fixed size 256 bits for example is then exchanged
via the aymmetric RSA.
-}

-------------------------------------------------------------------------------
-- PART 1 : asymmetric encryption

gcd :: Int -> Int -> Int
gcd m 0 = m
gcd m n = gcd n (m `mod` n)

phi :: Int -> Int
phi m = length $ filter (\x -> gcd x m == 1) [1..m]

-- Calculates (u, v, d) the gcd (d) and Bezout coefficients (u and v)
-- such that au + bv = d
computeCoeffs :: Int -> Int -> (Int, Int)
computeCoeffs a 0 = (1, 0) 
computeCoeffs a b = (v', u' - q * v')
  where (u', v') = computeCoeffs b $ a `mod` b
        q        = a `div` b

-- Inverse of a modulo m
inverse :: Int -> Int -> Int
inverse a m = u `mod` m
  where (u, v) = computeCoeffs a m

-- Calculates (a^k mod m)
modPow :: Int -> Int -> Int -> Int
modPow a 0 1  = 0
modPow a 0 m  = 1
modPow a k m
  | even k    = lowerModPow
  | otherwise = (a * lowerModPow) `mod` m
  where j = k `div` 2
        lowerModPow = modPow (a^2 `mod` m) j m

-- Returns the smallest integer that is coprime with x
smallestCoPrimeOf :: Int -> Int
smallestCoPrimeOf x = head $ filter (\y -> gcd x y == 1) [2..]

-- Generates keys pairs (public, private) = ((e, n), (d, n))
-- given two "large" distinct primes, p and q
genKeys :: Int -> Int -> ((Int, Int), (Int, Int))
genKeys p q = ((e, n), (d, n))
  where n = p * q
        e = smallestCoPrimeOf $ (p - 1) * (q - 1)
        d = inverse e $ (p - 1) * (q - 1)

-- RSA encryption/decryption
rsaEncrypt :: Int -> (Int, Int) -> Int
rsaEncrypt x (e, n) = modPow x e n

rsaDecrypt :: Int -> (Int, Int) -> Int
rsaDecrypt c (d, n) = modPow c d n

-------------------------------------------------------------------------------
-- PART 2 : symmetric encryption

-- Returns position of a letter in the alphabet
toInt :: Char -> Int
toInt x = ord x - ord 'a'

-- Returns the n^th letter
toChar :: Int -> Char
toChar x = chr $ ord 'a' + x

numberOfLettersInAlphabet :: Int
numberOfLettersInAlphabet = 26

-- "adds" two letters
add :: Char -> Char -> Char
add x y = toChar $ (toInt x + toInt y) `mod` numberOfLettersInAlphabet

-- "substracts" two letters
substract :: Char -> Char -> Char
substract x y = toChar $ (toInt x - toInt y) `mod` numberOfLettersInAlphabet

-- the next functions present
-- 2 modes of operation for block ciphers : ECB and CBC
-- based on a symmetric encryption function e/d such as "add"

-- ecb (electronic codebook) with block size of a letter
--
ecbEncrypt :: Char -> String -> String
ecbEncrypt k = map (add k)

ecbDecrypt :: Char -> String -> String
ecbDecrypt k = map (`substract` k)

-- cbc (cipherblock chaining) encryption with block size of a letter
-- initialisation vector iv is a letter
-- last argument is message m as a string
--

cbcEncrypt :: Char -> Char -> String -> String
cbcEncrypt k iv []     = []
cbcEncrypt k iv (x:xs) = c : cbcEncrypt k c xs
  where c = add (add iv x) k

cbcDecrypt :: Char -> Char -> String -> String
cbcDecrypt k iv []     = []
cbcDecrypt k iv (c:cs) = x : cbcDecrypt k c cs 
  where x = substract (substract c k) iv

-- affine cipher
affineEncrypt :: Int -> Int -> String -> String
affineEncrypt a b = map $ \x -> toChar $ (a * toInt x + b)
  `mod` numberOfLettersInAlphabet

affineDecrypt :: Int -> Int -> String -> String
affineDecrypt a b = map $ \x -> toChar $ 
  inverse a numberOfLettersInAlphabet * (toInt x - b)
    `mod` numberOfLettersInAlphabet
