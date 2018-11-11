-- Program that prints a text by reversing each word.

main = do
  s <- getContents
  let s' = process s
  putStr s'

process =
  unlines . map (unwords . map reverse . words) . lines
