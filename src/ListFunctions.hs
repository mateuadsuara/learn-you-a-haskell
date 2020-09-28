module ListFunctions
    ( head'
    , tail'
    , last'
    , init'
    , length'
    , null'
    , reverse'
    , take'
    , drop'
    , maximum'
    , minimum'
    , sum'
    , product'
    , elem'
    , cycle'
    , repeat'
    , replicate'
    , (+++)
    , quicksort
    , zipWith'
    , map'
    , filter'
    , takeWhile'
    ) where

head' :: [a] -> a
head' []     = error "empty list"
head' (x:xs) = x

tail' :: [a] -> [a]
tail' []     = error "empty list"
tail' (x:xs) = xs

last' :: [a] -> a
last' []     = error "empty list"
last' (x:[]) = x
last' (x:xs) = last' xs

init' :: [a] -> [a]
init' []       = error "empty list"
init' (x:[])   = []
init' (x:xs)   = x : init' xs

length' :: [a] -> Int
length' = foldr (const (+1)) 0

null' :: [a] -> Bool
null' [] = True
null' _  = False

reverse' :: [a] -> [a]
reverse' = _reverse' []
  where _reverse' :: [a] -> [a] -> [a]
        _reverse' acc []     = acc
        _reverse' acc (x:xs) = _reverse' (x : acc) xs

take' :: Int -> [a] -> [a]
take' _ []    = []
take' n l@(x:xs)
  | n <= 0    = []
  | otherwise = x : take' (n-1) xs

drop' :: Int -> [a] -> [a]
drop' _ []    = []
drop' n l@(x:xs)
  | n <= 0    = l
  | otherwise = drop' (n-1) xs

maximum' :: (Ord o) => [o] -> o
maximum' = foldr1 max

minimum' :: (Ord o) => [o] -> o
minimum' = foldr1 min

sum' :: (Num n) => [n] -> n
sum' = foldr (+) 0

product' :: (Num n) => [n] -> n
product' = foldr (*) 1

elem' :: (Eq e) => e -> [e] -> Bool
elem' item = foldr (\el -> (item == el ||)) False

cycle' :: [a] -> [a]
cycle' = _cycle' []
  where _cycle' :: [a] -> [a] -> [a]
        _cycle' _ []     = error "empty list"
        _cycle' [] l     = _cycle' l l
        _cycle' (x:xs) l = x : _cycle' xs l

repeat' :: a -> [a]
repeat' a = a : repeat' a

replicate' :: Int -> a -> [a]
replicate' n e
  | n <= 0    = []
  | otherwise = e : replicate' (n-1) e

(+++) :: [a] -> [a] -> [a]
(+++) = flip $ foldr (:)

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smaller ++ [x] ++ larger
  where smaller = (quicksort $ filter (<= x) xs)
        larger  = (quicksort $ filter (> x) xs)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
  | f x       = x : rest
  | otherwise = rest
  where rest = filter' f xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
  | f x       = x : takeWhile' f xs
  | otherwise = []
