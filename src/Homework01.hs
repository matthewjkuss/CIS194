module Homework01  (
  toDigits,
  toDigitsRev,
  doubleEveryOther,
  sumDigits,
  validate
) where

import Prelude (Integer, Bool, (==), (>=), (.), (*), (+), (++), div, mod, otherwise)

-- Implementation of common library functions

reverse :: [a] -> [a]
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) = f x : map f xs

concat :: [[a]] -> [a]
concat [] = []
concat (x:xs) = x ++ concat xs

-- The rest

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n >= 10 = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = [n]

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:xs) = x : 2*y : doubleEveryOtherRev xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

validate :: Integer -> Bool
validate = (\x -> x `mod` 10 == 0) . sumDigits . concat . map toDigits . doubleEveryOther . toDigits