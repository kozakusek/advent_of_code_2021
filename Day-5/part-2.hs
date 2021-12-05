import System.IO
import qualified Data.IntSet as IntSet

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ fmap iread (words input)
  
iread :: String -> Int
iread = read
  
solution :: [Int] -> Int
solution lst = IntSet.size $ points lst IntSet.empty IntSet.empty
  where
    points :: [Int] -> IntSet.IntSet -> IntSet.IntSet -> IntSet.IntSet
    points [] set0 setC = setC
    points (x:y:x':y':t) set0 setC = points t set0' setC'
      where
        set0' = IntSet.union line set0
        setC' = IntSet.union setC $ IntSet.intersection line set0
        line = fromLine (x,y) (x', y')
        fromLine :: (Int, Int) -> (Int, Int) -> IntSet.IntSet
        fromLine (x,y) (x', y') 
          | x == x' = IntSet.fromList [x * dim + b | b <- [(min y y')..(max y y')]]
          | y == y' = IntSet.fromList [a * dim + y | a <- [(min x x')..(max x x')]]
          | y  < y' = let d = if x < x' then (+) else (-) in 
            IntSet.fromList [(d x (b - y)) * dim + b | b <- [y..y']]
          | y' < y  = let d = if x' < x then (+) else (-) in
            IntSet.fromList [(d x' (b - y')) * dim + b | b <- [y'..y]]
    dim = maximum lst 

