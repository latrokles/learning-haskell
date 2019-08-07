-- chapter 06: higher order functions
-- functions that take functions as parameters and/or return functions!

-- curried functions
{-
    just about every function in haskell.

    haskell functions only take 1 parameter. In order to accept several parameters
    haskell uses currying (I think we had to do this manually in ML back in school)

    haskell returns curried functions.

    max 4 5 
    is actually is partially applying 4 to max and then returns a partially
    applied function that takes another number.
    
    this is why the type signatures look the way they do.
    :t max
    max :: (Ord a) => a -> a -> a

    which in turn actually means:
    max :: (Ord a) => a -> (a -> a)

    takes an a as parameter and returns a function that takes an a and returns 
    an a.

    the space between two functions is a function application

-}

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- let multTwoNumbersWithNine = multThree 9
-- let result = multTwoNumbersWithNine 2 3
-- by calling multThree with a single parameter Haskell applies 9 to the first
-- parameter and returns a partially applied function that "takes" two numbers
-- as parameters

-- functions can take and return functions
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- note that -> is right associative, but needed in the code above to make it
-- clear that the argument is a function (a -> a)

-- use as: myzpwt (+) [4,2] [2, 4]
myzpwt :: (a -> b -> c) -> [a] -> [b] -> [c]
myzpwt _ [] _ = []
myzpwt _ _ [] = []
myzpwt f (x:xs) (y:ys) = f x y : myzpwt f xs ys

-- takes a function and two arguments and returns the  function with the
-- arguments flipped
myflip :: (a -> b -> c) -> (b -> a -> c)
myflip f = g
    where g x y = f y x

-- some higher order functions

-- map: takes a function and a list and applies the function to every element
--      of the list, returning a new list of with the resulting values
-- map :: (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (x:xs) = f x : map f xs

-- an example
doubleAll :: (Num a) => [a] -> [a]
doubleAll xs = map (* 2) xs

-- filter: takes a predicate and a list and returns a list of the values that
--         satisfy the predicate
-- filter :: (a -> Bool) -> [a] -> [a]
-- filter _ [] = []
-- filter p (x:xs)
--      | px        = x : filter p xs
--      | otherwise = filter p xs
-- an example
removeOddNumbers :: [Int] -> [Int]
removeOddNumbers xs = filter even xs

-- another quicksort
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    let smallerSorted = qsort(filter (<=x) xs)
        biggerSorted = qsort(filter (>x) xs)
    in smallerSorted ++ [x] ++ biggerSorted

-- takeWhile: takes a predicate and a list, goes through list returning values
--            that satisfy the predicate, but stops once it finds a value that
--            does not.

-- lambdas!
-- anonymous functions, mostly always meant to be passed to a higher order function
--
-- created by starting with \
-- \<parameters> -> <function body>
-- e.g. let f = \xs -> length xs > 15

-- folds
{-
    while doing recursion on lists, we have a common pattern of going down to the
    empty list or singleton list as our base case. Given how common this pattern
    is, there's a couple of functions that encapsulate it: folds.

    like map, but reduce a list to some single value.

-}
-- foldl: (left fold) folds the list from the left
-- takes a function, a starting value and the list
sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs


-- foldr: (right fold), like foldl, but accumulator consumes values from the 
-- right
map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

-- foldl1: like foldl, but assumes the starting value is the 1st element of the list
-- foldr1: like foldr, but assumes the starting value is the last element of the list

-- scanl
-- scanr

-- function application with $
-- function composition
