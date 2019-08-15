-- chapter 09: making our own types and typeclasses
-- things just got real...
import qualified Data.Map as Map
-- algebraic data types
{-
    use Data to define a type!

    e.g.
    Data Bool = False | True

    Data <type> = <value constructor1> | ... | <value constructorN>

-}

-- let's define a shape type
-- Circle x, y, r
-- Rectangle x1, y1, x2, y2
-- this definition doesn't allow to print in ghci
-- data Shape = Circle Float Float Float | Rectangle Float Float Float Float
-- this one derives Show
data Shape = Circle Float Float Float | Rectangle Float Float Float Float deriving (Show)

-- let's define the surface of a shape
surface :: Shape -> Float
surface (Circle _ _ r) = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- for more on shapes see:
-- https://github.com/latrokles/learning-haskell/blob/master/lyahfgg/shapes.hs

-- Record syntax ... at last?

{-
    we could define a person type as:
    data Person = Person String String Int deriving (Show)

    -- then we can do functions to get the info out of this new Person type

    firstName :: Person -> String
    firstName (Person firstname _ _) = firstname

    lastName :: Person -> String
    lastName (Person _ lastname ) = lastname

    age :: Person -> Int
    age (Person _ _ age) = age

    -- and then have fun trying to remember which parameter is which... 
    -- is it lastname then firstname or the other way around?
-}

-- or we can use record syntax!
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , phoneNumber :: String 
                     } deriving (Show, Read)

-- then we can make a new Person with:
-- let p1 = Person {firstName="Frodo", lastName="Baggins", age=111, phoneNumber="?"}
-- no ordering required in fields, but they all have to be there.
-- this also creates lookup functions for the fields in the record (e.g. firstName)
-- let name = (firstName p1) ++ " " ++ (lastName p1)

-- Type Parameters
{-
    value constructors take values and produce a new value (as in the Person 
                                                           example above)

    type constructors take types as parameters to produce new types

    this is how maybe is implemented.
    data Maybe a = Nothing | Just a

    a is a type parameter

    "We usually use type parameters when the type that's contained inside the 
    data type's various value constructors isn't really that important for the
    type to work"

-}

data Vector a = Vector a a a deriving (Show)

vplus :: (Num t) => Vector t -> Vector t -> Vector t
vplus (Vector i j k) (Vector l m n) = Vector (i+l) (j+m) (k+n)

-- Derived Instances
{-
    we can manually make new types instances of a type class by implementing the
    functions defined by the type class.

    But Haskell can automatically make our type and instance of
    (Eq, Ord, Enum, Bounded, Show, Read) with the 'deriving' keyword.

    e.g:
    read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person
-}

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- Type Synonyms
{-
    type synonyms allow to give types different names so that we can use them
    in clearer and more meaningful ways.
    
    an example is String being a [Char]

-}

type PhoneNumber = String
type Name = String
type PhoneBook = [(Name, PhoneNumber)]


-- let phoneBook = [("bob", "444-4444"), ("eve", "555-5555")]
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name, pnumber) `elem` pbook

-- side note for Either a b  = Left a | Right b deriving (Eq, Ord, Read, Show)
-- use when interested in why or how some function failed
-- result type Either a b, where a is a type that can give us info on the failure
-- and b is the type of the successful computation.
--

-- locker example
-- import qualified Data.Map as Map (see top declaration)
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
      Nothing -> Left $ "Locker number " ++ show lockerNumber ++ "does not exist!"
      Just (state, code) -> if state /= Taken
                               then Right code
                               else Left $ "Locker " ++ show lockerNumber ++ " is already taken!"


