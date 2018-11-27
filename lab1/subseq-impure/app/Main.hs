-- pragmas
{-# LANGUAGE BangPatterns #-}

module Main where

-- imports
import Control.Monad.ST
import Data.Array.ST
import Data.Foldable as F
import Data.Sequence as S

-- process array
-- Dynamic Programming Approach: O(n^2) time/space
-- resource: https://www.geeksforgeeks.org/count-palindrome-sub-strings-string/
process :: Int -> String -> ST s (Integer)
process dim str = do
    !vec <- newArray ((1, 1), (dim, dim)) 0 :: ST s (STArray s (Int, Int) Integer)
    
    -- set elements in diagonal to 1
    F.forM_ [1..dim] $ \i -> writeArray vec (i,i) 1 

    -- compute reverse of string
    let !str' = reverse str

    -- cntPals (left=1) (right=dim)
    cntPals str str' 1 dim vec  
    where 
        cntPals :: String -> String -> Int -> Int -> STArray s (Int, Int) Integer -> ST s (Integer)
        cntPals [] [] _ _ _ = pure 0
        cntPals (x:xs) (y:ys) l r arr
            | l > r = pure 0    -- nothing found
            | l == r = pure 1   -- one element
            | otherwise = do    -- l < r
                ret <- readArray arr (l, r)
                case ret of
                    0 -> do
                        left  <- cntPals xs (y:ys) (l + 1) r arr
                        right <- cntPals (x:xs) ys l (r - 1) arr
                        middle <- cntPals xs ys (l + 1) (r - 1) arr
                        let !sum = left + right
                        let ans' = 1 + sum
                        let !ans'' = sum - middle
                        if (x == y) then (writeArray arr (l, r) ans') >> pure ans'
                        else (writeArray arr (l, r) ans'') >> pure ans''
                    ans -> pure ans

-- main
main :: IO ()
main = do
    -- get length of string from stdin
    dim <- getLine

    -- get string from stdin
    str <- getLine
    
    -- parse it as Int / use it to create array
    let dimInt = Prelude.read dim :: Int
    
    -- call process fn    
    print $ runST (process dimInt str) `mod` 20130401  

