module Main where

-- imports
import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Control.Monad.State
import qualified Data.Map as M

cntPalindromesMemo :: S.Seq Char -> State (M.Map String Integer)  Integer
cntPalindromesMemo S.Empty = pure 0
cntPalindromesMemo (_ S.:<| S.Empty) = pure 1
cntPalindromesMemo seq = do
    memo <- get
    let str = F.toList seq
    case M.lookup str memo of
      Just ans -> pure ans
      Nothing  -> case seq of
        ((a S.:<| as) S.:|> b) -> do
            subCntLeft <- cntPalindromesMemo (a S.<| as)
            subCntRight <- cntPalindromesMemo (as S.|> b)
            let subCnt = subCntLeft + subCntRight
            if (a == b) then do
                put $ M.insert str (1 + subCnt) memo
                pure $ 1 + subCnt
            else do
                subCntMiddle <- cntPalindromesMemo as
                put $ M.insert str (subCnt - subCntMiddle) memo
                pure $ subCnt - subCntMiddle
                             
-- main
main :: IO ()
main = do
    -- get user input: getLine
    -- apply fromList on getLine
    -- input :: Seq
    input <- S.fromList <$> getLine
    let ans = evalState (cntPalindromesMemo input) M.empty
    print $ ans `mod` 20130401
