-- chapter 05: recursion recursion recursion...
{-
    no loops in haskell
    
    most problems are solved recursively.

    keep edge (base) cases in mind, usually the scenario where a recursive
    application does not make sense

    decomponse problems into sub problems with solutions that can be built back
    up into the larger solution. solve the base case first.


-}
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

betterMaximum :: (Ord a) => [a] -> a
betterMaximum [] = error "maximum of empty list"
betterMaximum [x] = x
betterMaximum (x:xs) = max x (betterMaximum xs)

interleave :: [a] -> [a] -> [a]
interleave [] [] = []
interleave (x:xs) (y:ys) =
    x:y:interleave xs ys

replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0 = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : take' (n-1) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

myRepeat :: a -> [a]
myRepeat x = x:myRepeat x

myzip :: [a] -> [b] -> [(a, b)]
myzip _ [] = []
myzip [] _ = []
myzip (x:xs) (y:ys) = (x, y):myzip xs ys

myelem :: (Eq a) => a -> [a] -> Bool
myelem x [] = False
myelem x (y:ys)
    | x == y    = True
    | otherwise = myelem x ys

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted  = quicksort [a | a <- xs, a > x]
    in smallerSorted ++ [x] ++ biggerSorted

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

myzpwt :: (a -> b -> c) -> [a] -> [b] -> [c]
myzpwt _ [] _ = []
myzpwt _ _ [] = []
myzpwt f (x:xs) (y:ys) = f x y : myzpwt f xs ys


merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] xs = xs
merge (x:xs) (y:ys)
    | x <= y    = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys

mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = 
    let
        firstHalf = take (div n 2) xs
        secondHalf = drop (div n 2) xs
    in
        merge (mergesort firstHalf) (mergesort secondHalf)
    where
        n = length xs
