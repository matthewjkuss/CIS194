fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (-2+) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
  | even n = n + fun2 (n `div` 2)
  | otherwise = fun2 (3 * n + 1)

-- fun2' :: Integer -> Integer
fun2' = sum . filter (even) . 
  takeWhile (>1) . iterate (\n -> if even n then n `div` 2 else 3*n+1)

-- 12 + 6 + 10 + 16 + 8 + 4 + 2

data Tree a = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr (flip insert) Leaf

insert :: Tree a -> a -> Tree a
insert Leaf a = Node 0 Leaf a Leaf
insert (Node i l x r) a =
  if h l <= h r
    then 
      let nl = insert l a 
      in Node (1 + max (h nl) (h r)) nl x r
    else 
      let nr = insert r a 
      in Node (1 + max (h l) (h nr)) l x nr
  where 
    h (Node i _ _ _) = i
    h Leaf = -1

xor :: [Bool] -> Bool
xor = foldr (\new old -> if new then not old else old) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\new old -> f new : old) []


cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = foldr (\new old -> filter (\x -> x == new || x `mod` new /= 0) old) [2..2*n+2] [2..n]
