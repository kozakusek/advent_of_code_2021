import System.IO

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ fst3 $ solution (words input) (0, 0, 0)
  
iread :: String -> Int
iread = read
  
solution :: [String] -> (Int, Int, Int) -> (Int, Int, Int)
solution (h:h':t) (aim, x, y)
  | h == "forward" = solution t (aim, x + iread h', y + aim * iread h')
  | h == "down" = solution t (aim + iread h', x, y)
  | h == "up" = solution t (aim - iread h', x, y)

solution [] (_, x, y) = (x*y, 0, 0)

fst3 :: (a, b, c) -> a
fst3 (a, b, c) = a