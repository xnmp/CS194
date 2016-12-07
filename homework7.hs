-- {-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GADTs, FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Home7 where
import Data.Monoid
import Sized
import Data.Char
import qualified Data.Map as Map

data JoinList m a = Empty | Single m a | Append m (JoinList m a) (JoinList m a)
    -- Empty :: (Monoid m, Eq a) => JoinList m a
    -- Single :: (Monoid m, Eq a) => m -> a -> JoinList m a
    -- Append :: (Monoid m, Eq a) => m -> JoinList m a -> JoinList m a -> JoinList m a
    deriving (Eq, Show)

-- note that +++ is NOT associative
(+++) :: (Monoid m, Eq a, Eq m) => JoinList m a -> JoinList m a -> JoinList m a
jl1 +++ jl2
    | jl1 == Empty = jl2
    | jl2 == Empty = jl1
    | otherwise = Append (tag jl1 <> tag jl2) jl1 jl2

instance (Monoid m, Eq a, Eq m) => Monoid (JoinList m a) where
    mempty = Empty
    mappend a b = a +++ b

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single a _) = a
tag (Append a _ _) = a

data MaybeJ a = NothingJ | JustJ a deriving (Show, Eq)

-- need this for indexing
instance (Monoid a) => Monoid (MaybeJ a) where
    mempty = NothingJ
    mappend NothingJ NothingJ = NothingJ
    mappend NothingJ (JustJ a) = JustJ a
    mappend (JustJ a) NothingJ = JustJ a
    mappend (JustJ a) (JustJ b) = JustJ a
    

-- NOTE THE SIMILARITY HERE
-- could probably implement drop and take as some kind of adjoint

-- indexJ :: (Eq a, Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
-- -- the way this works is that the leftmost object has index 0
-- indexJ _ Empty = Nothing
-- indexJ i jl@(Single m a) = if i == 0 then Just a else Nothing
-- indexJ i jl@(Append m x y)
--     | i >= size_jl = Nothing
--     | i < size_x = indexJ i x
--     | i == size_x = indexJ 0 y
--     | otherwise = newres
--     where
--         newres = indexJ (i - size_x) y
--         size_x = getSize . size . tag $ x
--         size_jl = getSize . size . tag $ jl

foldJoinList :: (Sized b, Monoid b, Monoid c) => (Int -> JoinList b a -> c) -> Int -> JoinList b a -> c
foldJoinList f n jl@(Append m x y)
    | n >= size_x = f size_x x <> res_y -- this doesn't change the result but does speed it up
    | otherwise = res_x <> res_y
    where
        res_x = foldJoinList f (min n $ size_x) x
        res_y = foldJoinList f (max 0 $ n - size_x) y
        size_x = getSize . size . tag $ x
foldJoinList f n jl = f n jl

lll :: JoinList Size String
lll = Single (Size 1) "x" +++ Single (Size 1) "j" +++ Single (Size 1) "a" +++ Single (Size 1) "b"

-- this really should be a function Int -> b -> a -> c -> c -> c
-- thing is we already know how to combine the two cs and the b (with the monoid structure).
-- So it ends up being a function of just n and a tree.
-- It just needs to be defined for singletons and emptys, and when size equals n
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 0 jl = jl
dropJ n jl = Empty

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 jl = Empty
takeJ n jl = jl

-- indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> MaybeJ a
indexJ 0 jl@(Single m a) = JustJ a
indexJ n jl = NothingJ


-- Scrabble section
newtype Score = Score Int deriving (Show, Eq, Num)

instance Monoid Score where
    mempty = Score 0
    mappend (Score a) (Score b) = Score (a + b)

scoremap :: Map.Map Char Int
scoremap = Map.fromList
            [('E',1),('A',1),('I',1),('O',1),('N',1),('R',1),('T',1),('L',1),('S',1),('U',1),
            ('D',2),('G',2),('B',3),('C',3),('M',3),('P',3),
            ('F',4),('H',4),('V',4),('W',4),('Y',4),
            ('K',5),('J',8),('X',8),('Q',10),('Z',10)]

score :: Char -> Score
score ch = case Map.lookup (toUpper ch) scoremap of
    Nothing -> Score 0
    Just x -> Score x 

scoreString :: String -> Score
scoreString ss = sum $ map score ss

scoreLine :: String -> JoinList Score String
scoreLine ss = Single (scoreString ss) ss



main = print $  scoreLine "yay " +++ scoreLine "haskell!"