-- chapter 04: syntax in functions

-- pattern matching !!!
lucky :: (Integral a) => a -> String
lucky 7 = "Lucky number seven!"
lucky x = "Sorry you are out of luck :("


factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n -1)

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

-- use @ before the pattern to also keep a reference to the value, see @all below
capital :: String -> String
capital "" = "Empty string, whoops!"
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]


-- guards
{-
    testing a property and returning values based on whether the tests is True or False

    like an if statement but more versatile, more readable when there are more
    options and can be can be combined with patterns
-}

-- return value if condition is met, cascades through the conditions until it
-- reaches otherwise
bmiTell :: (RealFloat a) => a -> String
bmiTell bmi
    | bmi <= 18.5 = "under"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "over"
    | otherwise   = "wooo"

max' :: (Ord a) => a -> a -> a
max' a b
    | a > b     = a
    | otherwise = b

myCompare :: (Ord a) => a -> a -> Ordering
a `myCompare` b
    | a > b     = GT
    | a == b    = EQ
    | otherwise = LT

-- where clause to define bindings that can be used in the guard
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

-- where clause can use pattern matching
bmiTell2 :: (RealFloat a) => a -> a -> String
bmiTell2 weight height
    | bmi <= under  = "under"
    | bmi <= normal = "normal"
    | bmi <= over   = "over"
    | otherwise     = "wooo"
    where bmi    = weight / height ^ 2
          (under, normal, over) = (18.5, 25.0, 30)

-- can use where in functions directly (no guards), though straight up pattern 
-- matching alone should be enough
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calculateBmis :: (RealFloat a) => [(a, a)] -> [a]
calculateBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2


-- let !
{-
    similar to where bindings

    - where allows bindings be defined at the end so that the preceding function
      can use them.
    - where bindings are syntactic sugar, let bindings are expressions

    let allow binding anywhere, but local so they don't span across guards.

    let <bindings> in <expression>

-}
cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea  = pi * r ^2
    in sideArea + 2 * topArea

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi | (w,h) <- xs, let bmi = w / h ^2, bmi >= 25.0]

-- case expressions
{-
    case expressions

    case <expression> of <pattenr> -> <result>
                         <pattern> -> <result>

    can do pattern matching (ofc)
-}
describeList' :: [a] -> String
describeList' xs = "The list is " ++ what xs
    where what [] = "empty."
          what [x] = "a singleton list."
          what xs = "a longer list."

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."
