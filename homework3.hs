-- {-# OPTIONS_GHC -Wall #-}
module LogAnalysis where
    
import Data.List

skipsN :: Int-> Int -> [a] -> [a]
skipsN _ _ [] = []
skipsN 0 n (x:xs) = x:skipsN (n-1) n xs
skipsN m n (x:xs) = skipsN (m-1) n xs

skipsN2 = skipsN 0

skips ll = [skipsN2 n ll | n<-[1..length ll]]


localMaxima :: [Integer] -> [Integer]
localMaxima ll
    | length ll < 3 = []
localMaxima (q1:q2:q3:qs)
    | q2 > q1 && q2 > q3 = q2: nextlocal
    | otherwise = nextlocal
    where 
        nextlocal = localMaxima (q2:q3:qs)

counts :: [Int] -> Int-> Int
counts ll n = length . takeWhile (==n) . dropWhile (/=n) . sort $ ll

hist1 :: [Int] -> [String]
hist1 ll = map bigF [1..9]
    where
        bigF x = concat $ [f,g,h] <*> [x]
        f x = show x ++ "="
        g = flip replicate '*' . counts ll
        h x = replicate (m - counts ll x) ' '
        m = maximum $ map (counts ll) [1..9]

testlist = [1,4,5,4,6,6,3,4,2,4,9]
-- main = print $ hist1 testlist
main = putStr $ intercalate "\n" $ transpose $ map reverse $ hist1 [1,4,5,4,6,6,3,4,2,4,9]