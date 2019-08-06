-- chapter 03: types and typeclasses

-- type declarations, not always required.
-- use :t <function name> in ghci to see a function's type signature

removeNonUppercase :: String -> String -- read as functions takes a String and returns a String (?)
removeNonUppercase st = [ c | c <- st, c `elem` ['A'..'Z'] ]

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n -1)

{-
  some common types...

  Int
  Integer (like Int, but not bounded... so really big numbers)
  Float (single precision float)
  Double (double precision float)
  Bool
  Char
-}

{-
  type variables...

  :t head
  head :: [a] -> a

  a is a type variable, meaning head will take a list of any type and will return
  its first element. Which will be of type a (as input is a list of a)
  
  kinda like generics... polymorphism

  :t fst
  fst :: (a, b) -> a

  two type variables? tuples can contain multiple types so fst (which gets the
  first element of a tuple) uses 2 type variables. not a and b CAN be the same.

-}

-- typeclasses

{-

   typle class defines behavior for types that implement it (like an interface)

   :t (==)
   (==) :: (Eq a) => a -> a -> Bool
   
   => is a class constraint
   (==) takes any two values of the same type (a) and returns a Bool, but the value whatever
   the type of it MUST be a member of the Eq class
-}
