module Golf where

import Data.List


-- skips "ABCD" == ["ABCD", "BD", "C", "D"]
-- skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
-- skips [1] == [[1]]
-- skips [True,False] == [[True,False], [False]]
-- skips [] == []
skips :: [a] -> [[a]]
skips l = (\s -> snd <$> filter (\(n,_)->mod n s == 0) (zip [1..] l)) 
  <$> [1..length l]
-- skipsl=(\s->snd<$>filter(\(n,_)->modns==0)(zip[1..]l))<$>[1..lengthl]

-- Pointfree, for fun
-- skips = map (\(s,l) -> snd <$> filter ((==0) . (`mod` s) . fst) l) . zip [1..] . (\x -> zip [1..] . const x <$> x)

localMaxima :: [Integer] -> [Integer]
localMaxima l = 
  (\(_,x,_)->x) <$> (\(a,b,c) -> a < b && c < b) 
    `filter` zip3 l (tail l) (tail $ tail l)
-- localMaximal=(\(_,x,_)->x)<$>(\(a,b,c)->a<b&&c<b)`filter`zip3l(taill)(tail$taill)

histogram :: [Integer] -> String
histogram = (++"\n==========\n0123456789\n")
  . intercalate "\n" 
  . map (\r -> map (\x -> if elem x r then '*' else ' ') [0..9]) 
  . reverse 
  . transpose 
  . group 
  . sort
-- (++"\n==========\n0123456789\n").intercalate"\n".map(\r->map(\x->ifelemxrthen'*'else'')[0..9]).reverse.transpose.group.sort