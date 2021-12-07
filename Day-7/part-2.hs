import System.IO
import Data.List.Split (splitOn)
import GHC.Float (int2Double)


main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ fmap iread (splitOn "," input)

iread :: String -> Int
iread = read

solution :: [Int] -> Int
solution lst =  min a $ min b c 
  where
    m = round $ mean (map int2Double lst) 
    a = sum $ map ((\x -> (x * (1 + x)) `div` 2) . (abs . (\x -> x - m))) lst
    b = sum $ map ((\x -> (x * (1 + x)) `div` 2) . (abs . (\x -> x - m - 1))) lst
    c = sum $ map ((\x -> (x * (1 + x)) `div` 2) . (abs . (\x -> x - m + 1))) lst


mean :: [Double] -> Double
mean x = sum $ map (/ n) x
    where n = int2Double $ length x
