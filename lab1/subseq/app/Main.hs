module Main where

-- imports
import Data.Sequence

-- count palindromes
cntPalindromes :: Seq Char -> Integer
cntPalindromes Empty = 0
cntPalindromes (_ :<| Empty) = 1
cntPalindromes ((a :<| as) :|> b)
    | a == b     = (1 + subCnt) 
    | otherwise  = (subCnt - cntPalindromes as)
    where subCnt = cntPalindromes (a <| as) + cntPalindromes (as |> b)

-- main
main :: IO ()
main = do
    -- get user input: getLine
    -- apply fromList on getLine
    -- input :: Seq
    input <- fromList <$> getLine
    print $ cntPalindromes input
