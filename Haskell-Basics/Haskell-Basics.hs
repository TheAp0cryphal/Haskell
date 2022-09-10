--1
seqMultiple :: Int -> Int -> [Bool]
seqMultiple n m = [x `mod` m == 0 | x <- [1..n]]


--2
fib :: Int -> Int 
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

    
--3
listReverse :: [a] -> [a]
listReverse [] = []
listReverse (x:xs) = (listReverse xs) ++ [x]


--4
listAdd :: [Int] -> [Int] -> [Int]
listAdd [] [] = []
listAdd a [] = a
listAdd [] a = a
listAdd (x:xs) (y:ys) = x+y : (listAdd xs ys)

--5
inList :: Eq a => [a] -> a -> Bool
inList [] _ = False 
inList (x:xs) v = x == v || (inList xs v)

