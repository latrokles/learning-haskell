-- chapter 09: making our own types and typeclasses
-- things just got real...

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



