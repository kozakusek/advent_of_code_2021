import System.IO
import Data.List.Split (splitOn)
import GHC.Float (int2Double)
import Data.List (sort)

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ fmap iread (splitOn "," input)

iread :: String -> Int
iread = read

solution :: [Int] -> Int
solution lst = let m = round $ median (map int2Double lst) in
  sum $ map (abs . (\x -> x - m)) lst

median :: (Ord a, Fractional a) => [a] -> a
median x =
   if odd n
     then sort x !! (n `div` 2)
     else ((sort x !! (n `div` 2 - 1)) + (sort x !! (n `div` 2))) / 2
    where n = length x
