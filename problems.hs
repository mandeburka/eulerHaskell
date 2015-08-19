import Utils

pr001 t = sum [x | x <- [1..(t-1)], isMultiply x] where
    isMultiply x = any (\n -> (x `mod` n) == 0) [3, 5]

pr002 n = sum . filter even . takeWhile (<n) $ fibo

pr003 n = foldr max (minBound :: Int) (primeDividers n)

pr004 a b = foldr max (minBound :: Int) [x * y | x <- [a..b], y <- [x..b], isPalindrome (x * y)]
