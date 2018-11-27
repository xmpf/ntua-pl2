-- pragmas
{-# LANGUAGE TypeOperators #-}

module Main where

-- imports


data Tree a = T a [Tree a] deriving Show

-- foldr
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (T a b) = f a (foldTree f <$> b)

-- sizeTree t
sizeTree :: Num b => Tree a -> b
sizeTree = foldTree (\_ y -> 1 + sum y)

-- heightTree t
heightTree :: (Ord b, Num b) => Tree a -> b
heightTree = foldTree (\_ y -> 1 + maxi y)
    where   maxi [] = 0
            maxi ts = maximum ts

-- sumTree t
sumTree :: Num a => Tree a -> a
sumTree  = foldTree (\x y -> x + sum y)

-- maxTree t
maxTree :: Ord a => Tree a -> a
maxTree = foldTree (\x y -> maxi x y)
    where   maxi x [] = x
            maxi x (y : ys) = if (x > y) then x 
                                         else maxi y ys
-- inTree x t
inTree :: Eq a => a -> Tree a -> Bool
inTree = undefined

-- nodes t
nodes :: Tree a -> [a]
nodes (T t []) = [t]
nodes (T t ts) = foldl (++) [t] (map nodes ts)

-- countTree pred t
countTree :: (a -> Bool) -> Tree a -> Integer
countTree pred (T t []) = if (pred t) then 1 else 0
countTree pred (T t ts) = ta + foldr (+) 0 (map (countTree pred) ts)
                            where ta = if (pred t) then 1 else 0

-- leaves t
leaves :: Tree a -> [a]
leaves (T t []) = [t]
leaves (T t ts) = foldr (++) [] (map leaves ts)

-- mapTree f t
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

-- main
main :: IO ()
main = do
    let t1 = T 1 [ T 2 [ T 3 [], T 4 [ T 5 [ ], T 6 [ T 7 [] ] ] ] ]
    print $ heightTree t1
