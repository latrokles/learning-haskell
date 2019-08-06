
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry you are out of luck :("

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Cannot call head on an empty list!"
head' (x:_) = x

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "You are underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "would be good to lose some weight"
    | otherwise   = "overweight"

bmiTell' :: (RealFloat a) => a -> a -> String
bmiTell' weight height
    | bmi <= under  = "under"
    | bmi <= normal = "normal"
    | bmi <= over   = "over"
    | otherwise     = "wooo"
    where bmi    = weight / height ^ 2
          under  = 18.5
          normal = 25.0
          over   = 30


max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calculateBmis :: (RealFloat a) => [(a, a)] -> [a]
calculateBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^2
    in sideArea + 2 * topArea

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w / h ^2, bmi >= 25.0]

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."


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

