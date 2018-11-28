-- Preprocessor Directives

-- resources: https://www.cs.ox.ac.uk/ralf.hinze/publications/Bird.pdf
module Main where

-- Libraries
import Data.Maybe
import Test.QuickCheck

-- QuickCheck
--  | 
--      quickCheck :: Testable prop => prop -> IO ()
--  | 
--      (==>) :: Testable prop => Bool -> prop -> Property
--  | F
--      forAll :: (Show a, Testable prop) => Gen a -> (a -> prop) -> Property
--  | C
--      choose :: Random a => (a, a) -> Gen a 
--  | One of
--      oneof :: [Gen a] -> Gen a
--  | Frequency
--      frequency :: [(Int, Gen a)] -> Gen a
--  | Elements
--      elements :: [a] -> Gen a
--  | Sized test cases
--      sized :: (Int -> Gen a) -> Gen a
--  | Collects statistics of test cases
--      collect :: (Testable prop, Show a) => a -> prop -> Property

-- 
data Tree a = T a [Tree a] deriving Show
--

-- |                                |
-- |  QuickCheck Specific Functions |
-- |________________________________|
-- resource:  

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized genSizedTree

-- Generator function |= Restriction on size
genSizedTree :: Arbitrary a => Int -> Gen (Tree a)
genSizedTree n = do
    let depth = n `div` 2
    -- Test.QuickCheck choose :: Random a => (a, a) -> Gen a
    size <- choose (1, depth) 
    -- Test.QuickCheck vectorOf :: Int -> Gen a -> Gen [a]
    nodes <- vectorOf size (genSizedTree (depth `div` 2))
    -- Test.QuickCheck arbitrary :: Arbitrary a => Gen a
    root <- arbitrary
    return (T root nodes)
    

-- |                                |
-- |  General (Rose) Tree Functions |
-- |________________________________|
-- resource: https://hackage.haskell.org/package/containers-0.6.0.1/docs/Data-Tree.html


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


-- |                                |
-- | QuickCheck: Testing Properties |
-- |________________________________|

-- height > 0 && height <= size
prop_height :: (Eq a) => Tree a -> Bool
prop_height t = 
    (height > 0) && (height <= size)
    where
        height = heightTree t
        size   = sizeTree t

-- max value is in tree
prop_max :: (Ord a) => Tree a -> Bool
prop_max t = 
    (maxTree t) `inTree` t

-- nodes in tree
prop_nodesExist :: (Eq a) => Tree a -> Bool
prop_nodesExist t = undefined

-- count
prop_count :: (a -> Bool) -> Tree a -> Bool
prop_count f t =
    (countTree f t) <= (sizeTree t)

-- |nodes| = size && |leaves| < |nodes| unless both have size of 1
prop_nodes :: Tree a -> Bool
prop_nodes t 
    | (nodes_ == size_) && (leaves_ < nodes_) = True
    | (nodes_ == leaves_) && (nodes_ == 1) = True
    | otherwise = False
    where nodes_ = length (nodes t)
          leaves_ = length (leaves t)
          size_ = sizeTree t

-- mapTree preserves (size, height)
prop_mapTree :: (a -> b) -> Tree a -> Bool
prop_mapTree f t =
    let mt = mapTree f t
        size_ = sizeTree t
        height_ = heightTree t
    in ((sizeTree mt == size_) && (heightTree mt == height_))

-- if (val exist in t) then ((f val) exist in (mapTree f t))
prop_val :: (Eq a, Eq b) => (a -> b) -> Tree a -> a -> Bool
prop_val f t val =
    let mt = mapTree f t
        mv = f val
    in ((val `inTree` t) && (mv `inTree` mt))

-- map f . g == g . mapTree f
prop_fn :: (Eq a, Eq b) => (a -> b) -> Tree a -> Bool
prop_fn f t =
    let g  = nodes
        g' = leaves
        tn = (map f . g) t == (g . mapTree f) t
        tl = (map f . g') t == (g' . mapTree f) t
    in ( tn == tl) 


-- |                                    |
-- | Bird Tree: Infinite Data Structure |
-- |____________________________________|

bird :: Tree Rational
bird = T 1 [left, right]
    where   right = mapTree (+ 1) $ mapTree (^^ (- 1)) bird   -- (1 / x) + 1
            left = mapTree (^^ (- 1)) $ mapTree (+ 1) bird  -- 1 / (x + 1)


-- |                               |
-- | Bird Tree: QuickCheck Testing |
-- |_______________________________|


-- path
prop_path :: Int -> Bool
prop_path sz = undefined

-- zig zag (right - left)
prop_ziggy :: Int -> Bool
prop_ziggy depth = undefined

-- leftmost => [denominator] fibonacci sequence
prop_fibonacci depth = undefined

-- every rational exist in bird tree
-- uses auxilliary function findBird
prop_rationalExist :: Rational ->  Bool
prop_rationalExist = undefined

-- given a real number, return the path in the bird tree
findBird :: Rational -> [Int]
findBird = undefined

-- main
main :: IO ()
main = do
    print $ "Test"
