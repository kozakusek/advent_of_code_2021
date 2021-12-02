import System.IO

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ fst $ solution (words input) (0, 0)
  
iread :: String -> Int
iread = read
  
solution :: [String] -> (Int, Int) -> (Int, Int)
solution (h:h':t) (x, y)
  | h == "forward" = solution t (x + iread h', y)
  | h == "down" = solution t (x, y + iread h')
  | h == "up" = solution t (x, y - iread h')

solution [] (x, y) = (x*y, 0)
