module Homework01  (
  {- Ex.1 -} toDigits, toDigitsRev,
  {- Ex.2 -} doubleEveryOther,
  {- Ex.3 -} sumDigits,
  {- Ex.4 -} validate,
  {- Ex.5 -} Peg, Move, hanoi
) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n 
  | n >= 10 = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = [n]

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ls = conditionalDouble <$> zip doubleFlags ls
  where
    conditionalDouble (double, x) = if double then 2*x else x
    doubleFlags = cycle $
      if length ls `mod` 2 == 0 
      then [True, False] 
      else [False, True]

{-
Alternatively (and perhaps more simply) we can define:
  doubleEveryOtherRev :: [Integer] -> [Integer]
  doubleEveryOtherRev [] = []
  doubleEveryOtherRev (x:[]) = [x]
  doubleEveryOtherRev (x:y:xs) = x : 2*y : doubleEveryOtherRev xs
and then simple we have:
  doubleEveryOther = reverse . doubleEveryOtherRev . reverse
-}

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = x + sumDigits xs

validate :: Integer -> Bool
validate = 
    (\x -> x `mod` 10 == 0) 
  . sumDigits 
  . concatMap toDigits 
  . doubleEveryOther 
  . toDigits

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = 
     hanoi (n-1) a c b 
  ++ [(a, b)]
  ++ hanoi (n-1) c b a