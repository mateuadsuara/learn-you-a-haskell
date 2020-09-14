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
init' (x:y:[]) = x : []
init' (x:xs)   = x : init' xs

length' :: [a] -> Int
length' []     = 0
length' (x:xs) = 1 + length' xs

null' :: [a] -> Bool
null' [] = True
null' _  = False

_reverse' :: [a] -> [a] -> [a]
_reverse' acc []     = acc
_reverse' acc (x:xs) = _reverse' (x : acc) xs
reverse' :: [a] -> [a]
reverse' = _reverse' []

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
maximum' []     = error "empty list"
maximum' (x:[]) = x
maximum' (x:xs) = if x > max then x else max
  where max = maximum' xs

minimum' :: (Ord o) => [o] -> o
minimum' []     = error "empty list"
minimum' (x:[]) = x
minimum' (x:xs) = if x < min then x else min
  where min = minimum' xs

sum' :: (Num n) => [n] -> n
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: (Num n) => [n] -> n
product' []     = 1
product' (x:xs) = x * product' xs

elem' :: (Eq e) => e -> [e] -> Bool
e `elem'` []     = False
e `elem'` (x:xs) = if e == x then True else e `elem'` xs

_cycle' :: [a] -> [a] -> [a]
_cycle' _ []     = error "empty list"
_cycle' [] l     = _cycle' l l
_cycle' (x:xs) l = x : _cycle' xs l
cycle' :: [a] -> [a]
cycle' = _cycle' []

repeat' :: a -> [a]
repeat' a = a : repeat' a

replicate' :: Int -> a -> [a]
replicate' n e
  | n <= 0    = []
  | otherwise = e : replicate' (n-1) e
