--1

sumTailRec :: Num a => [a] -> a
sumTailRec [] = 0
sumTailRec (x:xs) = (sumTailRec xs) + x

--2

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f x [] = x
myFoldl f x (y:ys) = myFoldl f (f x y) ys

--3

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f x [] = x
myFoldr f x (y:ys) = f y (myFoldr f x ys)

--4

alternativeMap :: Num a => (a -> b) -> (a -> b) -> [a] -> [b]
alternativeMap f1 f2 [] = []
alternativeMap f1 f2 (x:xs) = f1 x : alternativeMap f2 f1 xs

--5

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f [] = []
myFilter f x = foldl (\a e -> if f e
    then a++ [e]
     else a) [] x

--6

sumsqeven'' :: [Integer] -> Integer
sumsqeven'' = sum . filter even . map (^2)