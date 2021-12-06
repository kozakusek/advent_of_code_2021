import System.IO
import Data.List.Split (splitOn)

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ fmap iread (splitOn "," input)
  
iread :: String -> Int
iread = read

solution :: [Int] -> Int
solution = foldl (\a e -> a + after80 e) 0


after80 :: (Eq a, Num a, Num p) => a -> p
after80 start 
  | start == 0 = 1421
  | start == 1 = 1401
  | start == 2 = 1191
  | start == 3 = 1154
  | start == 4 = 1034
  | start == 5 = 950
  | start == 6 = 905
  | start == 7 = 779
  | start == 8 = 768
  | otherwise = 0
