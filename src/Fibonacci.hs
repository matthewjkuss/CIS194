module Fibonacci where

-- EXERCISE 01

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- EXERCISE 02

fibs2 = fst <$> iterate next (0, 1)

next :: (Integer, Integer) -> (Integer, Integer)
next (prev, current) = (current, prev + current)

-- EXERCISE 03

data Stream a = Stream a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Stream x xs) = x : streamToList xs

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList
  

-- EXERCISE 04

streamRepeat :: a -> Stream a
streamRepeat x = Stream x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream x xs) = Stream (f x) (streamMap f xs)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Stream x (streamFromSeed f (f x))

-- EXERCISE 05

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream Integer -> Stream Integer -> Stream Integer
interleaveStreams (Stream l ls) (Stream r rs) = 
  Stream l (Stream r (interleaveStreams ls rs))


-- ruler x = interleaveStreams (streamRepeat x) (ruler (x+1))
-- ruler :: Stream Integer
-- ruler = streamFromSeed (interleaveStreams (streamRepeat 0))
  -- interleaveStreams 
  --   (streamRepeat 0) 
  --   (interleaveStreams (streamRepeat 1) (streamRepeat 2))

ruler :: Stream Integer
ruler = streamMap largestPowTwoFactor $ streamFromSeed (+1) 1

largestPowTwoFactor :: Integer -> Integer
largestPowTwoFactor n = 
    toInteger 
  $ (-1+)
  $ length
  $ takeWhile (snd) 
  $ iterate (\(x, _) -> (x `div` 2, x `mod` 2 == 0)) (n, True)