module Utils
( fibo
, dividers
, isPrime
, primeDividers
, isPalindrome
, primes
, windows
, lMax
, lMin
, pythagoreans
, isPythagorean
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


primes :: [Int]
primes = f [] where
  f :: [Int] -> [Int]
  f [] = 2 : f [2]
  f (x:xs) =
    let next = head . filter (\n -> all (\y -> n `mod` y /= 0) (x:xs)) $ [(x+1)..]
    in  next : f (next : x : xs)

windows :: Int -> [a] -> [[a]]
windows n (xs)
  | length xs <= n = [xs]
  | otherwise = take n xs : (windows n . tail $ xs)

lMax :: (Ord a, Bounded a) => [a] -> a
lMax = foldr max minBound

lMin :: (Ord a, Bounded a) => [a] -> a
lMin = foldr min maxBound

sqr:: Int -> Int
sqr x = truncate ((fromIntegral x :: Float) ** 2)

isPythagorean :: Int -> Int -> Int -> Bool
isPythagorean a b c
  | a < b && b < c = sqr a + sqr b == sqr c
  | otherwise = False

pythagoreans :: Int -> Int -> [[Int]]
pythagoreans start stop = [[a, b, c]| a <- [start..stop], b <- [(a+1)..stop], c <- [(b+1)..stop], isPythagorean a b c]
