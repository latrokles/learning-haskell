module Shapes
( Point(..)
, Shape(..)
, surface
, nudge
) where

data Point = Point Float Float deriving (Show)

-- define shapes in terms of points and dimensions
data Shape = Circle Point Float | Rectangle Point Point deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

-- move a shape
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) dx dy = Circle (Point (x+dx) (y+dy)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) dx dy =
    Rectangle (Point (x1+dx) (y1+dy)) (Point (x2+dx) (y2+dy))
