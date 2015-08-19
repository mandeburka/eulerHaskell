module Utils
( fibo
, dividers
, isPrime
, primeDividers
, isPalindrome
) where

fibo :: [Int]
fibo = 1 : f 1 2 where
    f a b = b : f b (a + b)

dividers :: Int -> [Int]
dividers x = [d | d <- [1..x], x `mod` d == 0]

truncatedSqrt :: Int -> Int
truncatedSqrt x = let
    s = sqrt . fromIntegral $ x :: Double
    in truncate s

isPrime :: Int -> Bool
isPrime x = takeWhile (<= truncatedSqrt x) (dividers x) == [1]

primeDividers :: Int -> [Int]
primeDividers x = filter isPrime (takeWhile (< truncatedSqrt x) (dividers x))


isPalindrome :: Int -> Bool
isPalindrome x = (read . reverse . show $ x) == x
