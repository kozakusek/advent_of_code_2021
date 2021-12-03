import System.IO
import Data.Bool(bool)
import Data.Char(digitToInt)

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution (words input)
  
solution :: [String] -> Int
solution lst = ogr * csr
  where
    ogr = bintodec $ getAll lst (>=0)
    csr = bintodec $ getAll lst (<0)
    
bintodec :: [Int] -> Int
bintodec lst = foldl (\a e -> 2*a + e) 0 lst

getAll :: [String] -> (Int -> Bool) -> [Int]
getAll [] _ = []
getAll (h:[]) _ = map digitToInt h
getAll strs pred = theOne:(getAll newStrs pred)
  where
    theOne = bool 0 1 $ pred $ getOne strs
    newStrs = map (tail) $ filter (\(h:t) -> theOne == digitToInt h) strs

getOne :: [String] -> Int
getOne ((h:_):t) = (digit h) + (getOne t)
getOne _ = 0

digit :: Char -> Int
digit '0' = -1
digit  _  = 1
