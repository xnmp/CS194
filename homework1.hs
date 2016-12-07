import Data.Char

toDigits n = digitToInt <$> show n

double _ [] = []
double 0 (x:xs) = x : double 1 xs
double 1 (x:xs) = 2*x : double 0 xs

double2 ll = reverse $ double 0 $ reverse ll

combine ll = read $ concat $ show <$> ll :: Int

resum :: Int -> Int
resum = sum .toDigits . combine . double2 . toDigits

res n = resum n `mod` 10 == 0

main = print $ resum $ 8765