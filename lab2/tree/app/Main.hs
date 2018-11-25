-- pragmas
{-# LANGUAGE TypeOperators #-}

module Main where

-- imports


data Tree a = T a [Tree a] deriving Show

-- foldr
foldTree :: (a -> [b] -> b) -> Tree a -> b
foldTree f (T a b) = f a (foldTree f <$>  b)

-- sizeTree t
sizeTree :: Num b => Tree a -> b
sizeTree (T _ []) = 1
sizeTree (T _ ts) = 1 + (foldr (+) 0 $ map sizeTree ts)
            

-- heightTree t
heightTree :: (Ord b, Num b) => Tree a -> b
heightTree (T _ []) = 1
heightTree (T _ ts) = 1 + maximum (map heightTree ts)

-- sumTree t
sumTree :: Num a => Tree a -> a
sumTree (T t []) = t
sumTree (T t ts) = t + (foldr (+) 0 $ map sumTree ts)

-- maxTree t
maxTree :: Ord a => Tree a -> a
maxTree (T t []) = t
maxTree (T t ts) =
    if (t > t') then t else t'
        where t' = maximum (map maxTree ts)

-- inTree x t
inTree :: Eq a => a -> Tree a -> Bool
inTree val (T t []) = 
    if (val == t) then True 
                  else False
inTree val (T _ ts) = foldr (||) False (map (inTree val) ts)


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
    let t1 = T 1 [ T 2 [ T 3 [], T 4 [ T 5 [ ], T 6 [ ] ] ] ]
    print $ mapTree (*2) t1
