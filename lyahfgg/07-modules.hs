-- chapter 07: modules
{-
    collect related functions. modules !

    encapsulate and group code

    most stuff so far comes from Prelude module, imported by default
    (kinda like Ruby's Kernel)

    import a module with:

    import <module name>

-}
import Data.List -- functions for working with lists
{-
    the import above is like a .* import in Java, gives you everything and the
    kitchen sink in Data.List. To get specific functions use:
    
    import <module name> (<function1>, <function2>).

    e.g.:
    import Data.List (nub, sort)

    to import everything except a few functions use:

    import <module name> hiding (function_we_do_not_want)

    e.g:

    import Data.List hiding (nub)
-}

-- there are also qualified imports s.a:
import qualified Data.Map
import qualified Data.Map as Map
import qualified Data.Set as Set
-- we can then use Data.Map's filter thus
-- Data.Map.filter

-- can also alias imports with
-- import qualified <module name> as <alias>
-- import qualified Data.Map as M
-- then
-- M.filter

-- return number of unique elements in a list
numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub

-- haskell's standard lib reference
-- https://downloads.haskell.org/~ghc/latest/docs/html/libraries/

-- can also search via Hoogle (heh)
-- supports name, module, type signature
-- https://hoogle.haskell.org/

-- for ghci use :m <module name>

-- come back to: http://learnyouahaskell.com/modules#data-list or look up Hoogle
-- for Data.List functions

-- come back to:
-- http://learnyouahaskell.com/modules#data-list

-- Data.Map
-- http://learnyouahaskell.com/modules#data-map
{-
    associative lists (dictionaries, hashmaps, key value stores)

    can be represented as a list of pairs (key, value).

-}

-- will fail if key does not exist
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k, v) -> key == k) $ xs


-- first time we see Nothing and Just, which are of the Maybe data type
betterFindKey :: (Eq k) => k -> [(k,v)] -> Maybe v
betterFindKey key [] = Nothing
betterFindKey key ((k,v):xs) = if key == k
                                  then Just v
                                  else betterFindKey key xs

findKeyWithFold :: (Eq k) => k -> [(k,v)] -> Maybe v 
findKeyWithFold key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing

-- DataMap has association lists that are more efficient than the above list of pairs
-- as they are implemented with trees
-- let ourMap = Map.fromList [("latrokles", "555-55555"), ("foo", "111-1111")]

-- Data.Set
-- see: http://learnyouahaskell.com/modules#data-set for functions, or lookup Hoogle
{-
    Sets... list of unique values... in Haskell we get Ordered Sets (due to their
    tree based implementation)

    most common operations:
    - adding things
    - checking for membership
    - converting Set to a list

    create a set from a list
    let set = Set.fromList "sdasdasdssada" -> fromList "ads"
-}

-- Making our own Modules
{-
    create a file for our module, mymodule.hs

    module <module name>
    ( <function1>
    , <function2>
    ...
    , <functionN>
    ) where

    function1 = ...
    function2 = ...
    ...
    functionN = ...

    the module name can be more specific... say:
    -- Sphere.hs
    module Geometry.Sphere
    ( volume
    , area
    ) where ...

    and 
    
    -- Cuboid.hs
    module Geometry.Cuboid
    ( volume
    , area
    ) where ...

-}
