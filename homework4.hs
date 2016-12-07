-- {-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import           Data.List

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
    | even x = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x->x-2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n
    | even n = n + fun2 (n `div` 2)
    | otherwise = fun2 (3 * n + 1)

fun2' = sum . filter even . takeWhile (/=1) . iterate (\n->if even n then n`div`2 else 3*n+1)


data Tree a = Leaf | Node {height:: Integer, leftTree::Tree a, val::a, rightTree:: Tree a}
    deriving (Show, Eq)

isFull :: Tree a -> Bool
isFull Leaf = True
isFull (Node _ l _ r) = ((treeHeight l) == (treeHeight r)) && isFull l && isFull r

treeHeight :: Tree a -> Integer
treeHeight Leaf = -1
treeHeight x    = height x

treeIns :: a -> Tree a -> Tree a
treeIns x Leaf = Node 0 Leaf x Leaf
treeIns x t@(Node h left y right)
    | isFull t = Node (h+1) (treeIns x left) y right
    | isFull left = Node h left y (treeIns x right)
    | otherwise = Node h (treeIns x left) y right


treeBuild :: [a] -> Tree a
treeBuild = foldr treeIns Leaf

xor :: Bool -> Bool -> Bool
xor a b
    | a == b = False
    | otherwise = True

xorList :: [Bool] -> Bool
xorList = foldl1 xor

map' :: (a->b) -> [a] -> [b]
map' f = foldr (\a1 b1-> f a1 : b1) [] 

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x->2*x+1) ([1..n]\\[i+j+2*i*j | i<-[1..n], j<-[1..n]])

-- main = print $ hist1 testlist
main = print $ sieveSundaram 1000
