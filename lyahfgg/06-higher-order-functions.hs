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
-- implementing last with foldl1
-- begins on the left end of the list (1st element), ignoring the accumulator and
-- returning the value of the element, terminating on the last element
last' :: [a] -> a
last' = foldl1 (\_ x -> x)

-- foldr1: like foldr, but assumes the starting value is the last element of the list
-- implementing head with foldr1
-- starts on the right end of the list (last element), ignoring the accumulator and
-- returning the value of the element, terminating on the first element
head' :: [a] -> a
head' = foldr1 (\x _ -> x)

-- scanl and scanr
{-
    like foldl and foldr, but return the values of the accumulator states, e.g.:
    - scanl (+) 0 [3,5,2,1] --> [0,3,8,10,11]
    - scanr (+) 0 [3,5,2,1] --> [11,8,3,1,0]

    there's also foldl1 and fodlr1

    scans can be used to monitor the progression of a function that can be
    implemented as a fold.

    e.g. how many elements does it take for the sum of the roots of all natural
    numbers to exceed 1000
-}
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function application with $
{-
    $ = function appliction (inline)

    ($) :: (a -> b) -> a -> b
    f $ x = f x

    normal function application (empty space):
    - has highest precedence
    - left associative e.g. f a b c --> (((f a) b) c)

    $ function application: 
    - has lowest precedence
    - right associative e.g. f $ a $ b $ c --> (f (a (b c)))

    $ helps us make things more readable by not using so many parens

    sum (map sqrt [1..130]) vs. sum $ map sqrt [1..130]

    sqrt 3 + 4 + 9   --> (sqrt 3) + 4 + 9
    sqrt $ 3 + 4 + 9 --> sqrt (3 + 4 + 9)

    or a more complex one:

    sum (filter (> 10) (map (*2) [2..10])) == sum $ filter (>10) $ map (*2) [2..10]

    you can even treat $ as any other function
    map ($ 3) [(4+), (10*), (^2)] --> [7, 30, 9]

-}

-- function composition
{-
    the . operator (.)

    function composition

    (.) :: (b -> c) -> (a -> b) -> a -> c
    f . g = \x -> f (g x)

    clearer way to making functions on the fly (w/out lambdas)

    map (\x -> negate (abs x)) [1, 2, -3, -4]

    is the same as:

    map (negate . abs) [1, 2, -3, -4]

    if functions take several parameters would have to be partially applied so
    that each function takes one paramters. e.g:

    sum (replicate 5 (max 6 9))

    becomes:

    (sum . replicate 5 . max 6) 9

    (.) is also used to defining functions in POINT FREE STYLE...

    point free is a way to write functions in a way that they do not explicitly
    state the parameters they take. This is made possible because of currying in
    haskell functions. For exmample:
-}

-- point free version of sum' xs = foldl (+) 0 xs
pointFreeSum :: (Num a) => [a] -> a
pointFreeSum = foldl (+) 0

-- what's the point free version of:
-- fn x = ceiling (negate (tan (cos (max 50 x))))
fn = ceiling . negate . tan . cos . max 50

-- thinking in terms of functions instead of the data.
-- avoid long chains of compositions in order to keep things readable
