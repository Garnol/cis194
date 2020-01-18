{-# OPTIONS_GHC -Wall #-}

import Data.List
import Text.Printf

--------------------------------
---------- Exercise 1 ----------
--------------------------------

-- Select all the even, substract them by 2, then mutify them.
fun1 :: [Integer] -> Integer
fun1 = product . map (flip (-) 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (== 0) . iterate fun2Gen 
       where fun2Gen :: Integer -> Integer
             fun2Gen 1 = 0
             fun2Gen n = if even n
                         then n `div` 2
                         else 3 * n + 1


--------------------------------
---------- Exercise 2 ----------
--------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Generate all possible paths of length as input
-- A path is an binary string which figures out how to insert a node into a tree.
-- Example:
-- genPath 3 = ["001","010","011","100","101","110","111"]
genPath :: Integer -> [String]
genPath 0 = []
genPath len = map helper [1..(2^len-1)]
              where helper :: Integer -> String
                    helper x = printf ("%0" ++ (show len) ++ "b") x

-- Insert a node into a tree according to given path.
-- Start from the root, 0 for left, 1 for right.
-- Example:
-- insertTree 'A' 0 "" Leaf = Node 0 Leaf 'A' Leaf
-- insertTree 'B' 1 "0" (Node 0 Leaf 'A' Leaf) ==
--   Node 1
--        (Node 0 Leaf 'B' Leaf)
--        'A'
--        Leaf
-- insertTree 'C' 1 "1" (Node 1 (Node 0 Leaf 'B' Leaf) 'A' Leaf) ==
--   Node 1
--        (Node 0 Leaf 'B' Leaf)
--        'A'
--        (Node 0 Leaf 'C' Leaf)
insertTree :: a -> Integer -> String -> Tree a -> Tree a
insertTree content _ [] _ = Node 0 Leaf content Leaf
insertTree content _ _ Leaf = Node 0 Leaf content Leaf
insertTree content depth (dire:path) (Node _ ls now rs)
  | dire == '0' = Node depth (insertTree content (depth-1) path ls) now rs
  | dire == '1' = Node depth ls now (insertTree content (depth-1) path rs)
  | otherwise = Leaf

-- Generates a balanced binary tree from a list of values using foldr
-- Example:
-- foldTree "ABCDEFGHIJ" ==
--   Node 3
--     (Node 2
--       (Node 0 Leaf ’F’ Leaf)
--       ’I’
--       (Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
--   ’J’
--   (Node 2
--     (Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
--     ’H’
--     (Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))
foldTree :: [a] -> Tree a
foldTree xs = foldr helper Leaf $ zip xs paths
              where len = genericLength xs
                    paths = reverse $ genericTake len $ "" : (concat $ map genPath [0..len])
                    helper (x, path) tree = insertTree x (genericLength path) path tree


--------------------------------
---------- Exercise 3 ----------
--------------------------------
-- Returns True if and only if there are an odd number of True
-- values contained in the input list
xor :: [Bool] -> Bool
xor = foldr (\x y -> if x == y then False else True) False

-- Implement map as a fold
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Input n, generate all the odd prime numbers up to 2n + 2.
sieveSundaram :: Integer -> [Integer]
sieveSundaram x = foldl step [2..2*x+2] [2..x]
                  where step acc now = [k | k <- acc, (k == now) || (k `mod` now /= 0)]