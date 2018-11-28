-- pragmas
{-# LANGUAGE TypeOperators #-}

module Main where

-- imports
import Data.Maybe

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
inTree x t = x `elem` foldTree (\a as -> a : concat as) t

-- nodes t
nodes :: Tree a -> [a]
nodes = foldTree (\x xs -> x : concat xs)

-- countTree pred t
countTree :: (a -> Bool) -> Tree a -> Integer
countTree pred t = cnt $ map pred $ foldTree (\a as -> a : concat as) t
    where
        cnt [] = 0
        cnt (True : x) = 1 + cnt x
        cnt (_ : x) = cnt x

-- leaves t
leaves :: Tree a -> [a]
leaves t = foldTree (\x xs -> (if (null xs) then [x] else []) ++ concat xs) t


-- mapTree f t
mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree (\t ts -> T (f t) ts)

--                       --
-- == NOT IMPLEMENTED == --
--                       --

-- trimTree n t
trimTree :: Int -> Tree a -> Tree a
trimTree n t = undefined

-- path l t
path :: [Int] -> Tree a -> Maybe a
path l t = undefined 
        

-- main
main :: IO ()
main = do
    -- testing
    let t1 = T 1 [ T 2 [ T 3 [], T 4 [ T 5 [ ], T 6 [ T 7 [] ] ] ] ]
    print $ trimTree 2 t1
