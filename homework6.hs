-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module LogAnalysis where

import           Data.List

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs1 :: Integer -> [Integer]
fibs1 n = [fib n | n<-[1..n]]


fibs2 :: Integer -> [Integer]
fibs2 0 = [1]
fibs2 1 = [1,1]
fibs2 n = (head old + (head $ tail old)) : old
    where
        old = fibs2 (n-1)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x s) = x:streamToList s

instance {-# OVERLAPPABLE #-} (Show a)=> Show (Stream a) where
    show x = show ll ++ " and so on..."
        where
            ll = take 20 . streamToList $ x

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x sr) = Cons (f x) (streamMap f sr)

instance Functor Stream where
    fmap f (Cons x sr) = Cons (f x) (fmap f sr)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

nats :: Stream Integer
nats = streamFromSeed (+1) 1

ruler :: Stream Integer
ruler = fmap bigdiv nats
    where
        bigdiv n = (head $ dropWhile (\x -> n `mod` 2^x == 0) [0..]) - 1



type Polynomial = Stream Integer

instance {-# OVERLAPPING #-} Show Polynomial where
    show x = (concat $ fmap f $ zip ll xs) ++ "..."
        where
            f (x, y)
                | x == 0 = ""
                | y == "1" = show x ++ " + "
                | x == 1 = y ++ " + "
                | otherwise = show x ++ y ++ " + "
            xs = ["1"] ++ ["x"] ++ ["x^" ++ show n | n<-[2..]] 
            ll = take 20 . streamToList $ x

x :: Polynomial
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num Polynomial where
    fromInteger n = Cons n (streamRepeat 0)
    negate poly = fmap negate $ poly
    Cons a1 str1 + Cons a2 str2 = Cons (a1+a2) (str1 + str2)
    Cons a0 a' * b@(Cons b0 b') = Cons (a0*b0) (fmap (*a0) b' + a'*b)

instance Fractional Polynomial where
    Cons a0 a'/ Cons b0 b' = Cons (a0 `div` b0) remm
        where
            remm = fmap (`div` b0) $ a' + negate (Cons a0 a' / Cons b0 b' * b')

-- some roundabout shit to get it to dislay as a list and not a polynomial
fibs3 :: Stream Int
fibs3 = fmap fromInteger (x / (1 - x - x^2))

data Matrix a = Matrix a a a a deriving Show

instance Num a=>Num (Matrix a) where
      Matrix a b c d * Matrix e f g h = Matrix (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)

fib4 :: Integer -> Integer
fib4 n = getFib $ Matrix 1 1 1 0 ^ n
    where 
        getFib (Matrix _ x _ _) = x

main = print $ length $ show $ fromIntegral $ fib4 10000000
