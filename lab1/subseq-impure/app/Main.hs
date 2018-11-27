-- pragmas
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

-- imports
import Control.Monad.ST
import Data.Array.ST
import qualified Data.Foldable as F
import qualified Data.Sequence as Seq

-- Sequence views (containers @grader is out of date)
pattern Empty   <- (Seq.viewl -> Seq.EmptyL)  where Empty = Seq.empty
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs) where (:<)  = (Seq.<|)
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x) where (:>)  = (Seq.|>)

-- process array
-- Dynamic Programming Approach: O(n^2) time/space
-- resource: https://www.geeksforgeeks.org/count-palindrome-sub-strings-string/
process :: Int -> Seq.Seq Char -> ST s (Integer)
process dim str = do
    !vec <- newArray ((1, 1), (dim, dim)) 0 :: ST s (STArray s (Int, Int) Integer)
    
    -- set elements in diagonal to 1
    F.forM_ [1..dim] $ \i -> writeArray vec (i,i) 1 

    -- compute reverse of string

    -- cntPals (left=1) (right=dim)
    cntPals str 1 dim vec  
    where 
        cntPals :: Seq.Seq Char -> Int -> Int -> STArray s (Int, Int) Integer -> ST s (Integer)
        cntPals Empty _ _ _ = pure 0
        cntPals (_ :< Empty) _ _ _ = pure 1
        cntPals ((x :< xs) :> y) l r arr
            | l > r = pure 0    -- nothing found
            | l == r = pure 1   -- one element
            | otherwise = do    -- l < r
                !ret <- readArray arr (l, r)
                case ret of
                    0 -> do
                        -- drop last char
                        !left  <- cntPals (x :< xs) l (r - 1) arr
                        -- drop first char
                        !right <- cntPals (xs :> y) (l + 1) r arr
                        -- eager computation
                        let !sum = left + right
                        let !ans' = 1 + sum
                        
                        if (x == y) then do
                            writeArray arr (l, r) ans'
                            pure ans'
                        else do
                            -- drop both first and last chars
                            !middle <- cntPals xs (l + 1) (r - 1) arr
                            let !ans'' = sum - middle
                            writeArray arr (l, r) ans''
                            pure ans''

                    ans -> pure ans

-- main
main :: IO ()
main = do
    -- get length of string from stdin
    dim <- getLine

    -- get string from stdin
    str <- Seq.fromList <$> getLine
    
    -- parse it as Int / use it to create array
    let dimInt = Prelude.read dim :: Int
    
    -- call process fn    
    print $ runST (process dimInt str) `mod` 20130401  

