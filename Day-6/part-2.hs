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
solution = foldl (\a e -> a + after256 e) 0


after256 :: (Eq a, Num a, Num p) => a -> p
after256 start 
  | start == 0 = 6703087164
  | start == 1 = 6206821033
  | start == 2 = 5617089148
  | start == 3 = 5217223242
  | start == 4 = 4726100874
  | start == 5 = 4368232009
  | start == 6 = 3989468462
  | start == 7 = 3649885552
  | start == 8 = 3369186778
  | otherwise = 0
