-- chapter 02: starting out

{-
   Some functions to start out. launch ghci and load file with

   :load 02-starting-out.hs

   then execute the loaded functions directly on ghci

   function names cannot begin with uppercase letters, but can have ' (which is used to denote eagerly evaluated functions... oh yeah, Haskell is lazy...)
-}

-- simple function definition
doubleMe x = x + x

doubleUs x y = doubleMe x + doubleMe y

-- if... (else branch always required and must evaluate to same types)
doubleSmallNumber x = if x > 100
                         then x
                         else doubleMe x


{-
    lists seem to be bread and butter of data structures in haskell land (are they?)

    use let to bind values to identifiers in ghci
    let lostNumbers = [1, 5, 678, 2332, 123, 1] 

    some things about lists:
    - must contain values of the same type
    - Strings are really lists of Char (acters)
    - appending lists with ++ can get expensive
-}
addTwoLists x y = x ++ y

-- the cons operator : to append to the front of a list (I miss this from ML back in school)
appendToTheFrontOfList x aList = x:aList

-- use !! to get element by index
getElementByIndex index aList = aList !! index

{-
    some operations for lists
    
    head - first element
    tail - everything but the first element
    last - last element
    init - evverything but the last element
    length
    null - True if empty, otherwise False
    reverse
    take - returns the first n first elements of a list
    drop - returns a list without the first n elements of a list
    maximum - returns biggest element
    minimum - self explanatory
    sum - sum of its elements takes [a] where a is Num
    product
    elem - checks membership in list, True if x is in list... inline function

-}

-- take
giveMeFirstFiveElements aList = take 5 aList

-- drop
dropTheFirstFiveElements aList = drop 5 aList

-- elem
containsElement x aList = x `elem` aList

{-
    ranges
    [start..end] inclusive

    anything that can be enumerated ['a'..'z'] == "abcdefghijklmnopqrstuvwxyz"

    can specify a step (implicitly)
    [2,4..20] == [2,4,6,8,10,12,14,16,18,20]

    [20,19..1] instead of just [20..1] or else it won't work

    ranges of floats can be a pain due to imprecision of floating point

    because of Haskell's lazy evaluation... [1..] is totally ok as the range will
    be evaluated as needed. Say: take 24 [1..] will only evaluate the first 24 values.

    - cycle will repeat a list into an infinite list
    - repeat will repeat an element into an infinite list
    - replicate will repeat element n number of times
-}

-- list comprenhensions
doubleIt aList = [x*2 | x <- aList] -- return a list of x*2 for every x in aList

-- list comprenhension with predicates
onlyOdds aList = [x | x <- aList, x `mod` 2 /= 0]

-- multiple predicates, must match all predicates
filterDivisibleBy3and5 aList = [x | x <- aList, x `mod` 3 /= 0, x `mod` 5 /= 0]

-- multiple lists
multiplyTwoLists aList bList = [x*y | x <- aList, y <- bList]

-- implement length
length' xs = sum [1 | _ <- xs] -- we don't care much for the values so we use _ as placeholder

-- a nested one
-- let xxs = [[1,3,5,2,3,1,2,4,5],[1,2,3,4,5,6,7,8,9],[1,2,4,2,1,6,3,1,3,2,3,6]]
filterEvenNumbers xxs = [ [x | x <- xs, even x] |  xs <- xxs] -- for every sublist get every x that is even

-- tuples
{-
    grouping of several values
    can be of multiple types
    can have list of tuples but their type signature must be the same (i.e. same lenght with same types in same order)
    
    - fst - first element
    - snd - second element
    - no more positional syntatic sugar

    - zip will take two lists and combine their elements in tuples
-}
enumerateLowercaseAlphabet = zip [1..26] ['a'..'z']
