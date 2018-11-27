-- pragmas
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Main where

-- imports
import qualified Data.Sequence as Seq
import qualified Data.Foldable as F
import qualified Data.Map.Strict as M
import Control.Monad.State

-- Sequence views
pattern Empty   <- (Seq.viewl -> Seq.EmptyL)  where Empty = Seq.empty
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs) where (:<)  = (Seq.<|)
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x) where (:>)  = (Seq.|>)


-- Memoization using pure Map
cntM :: Seq.Seq Char -> State (M.Map String Integer) Integer
cntM Empty = pure 0
cntM (_ :< Empty) = pure 1
cntM seq = do
    memo <- get
    let str = F.toList seq
    case M.lookup str memo of
        Just ans -> pure ans
        Nothing -> case seq of
            ((x :< xs) :> y) -> do
                subL <- cntM (x :< xs)
                subR <- cntM (xs :> y)
                let subCnt = subL + subR
                if (x == y) then do
                    let ans' = 1 + subCnt
                    put $ M.insert str ans' memo
                    pure ans'
                else do
                    subM <- cntM xs
                    let ans'' = subCnt - subM
                    put $ M.insert str ans'' memo
                    pure ans''

-- main
main :: IO ()
main = do
    -- we don't really need length of string
    getLine
    input <- Seq.fromList <$> getLine
    let ret = evalState (cntM input) M.empty
    print $ ret `mod` 20130401
