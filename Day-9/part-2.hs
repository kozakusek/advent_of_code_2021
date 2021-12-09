import System.IO
import Data.Char (digitToInt)
import Data.List (sortOn)
import Data.Ord (Down(Down))
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

type Point = (Int, Int)

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ (parse input)

iread :: String -> Int
iread = read

solution :: Map.Map Point Int -> Int
solution grid = (product $ take 3 $ sortOn Down $ 
  map (Set.size . Set.fromList . basin grid) $ Map.keys grid)

parse :: String -> Map.Map Point Int
parse = (\rows -> Map.fromList 
    [((r, c), digitToInt n) | (r, row) <- zip [0..] rows, (c, n) <- zip [0..] row]) . lines

bfs :: (Ord a, Eq a) => (a -> Seq.Seq a) -> a -> [a]
bfs step start = rec Set.empty (Seq.singleton start)
  where seen Seq.Empty = []
        rec seen (cur Seq.:<| rest)
          | cur `Set.member` seen = rec seen rest
          | otherwise = cur : rec (Set.insert cur seen) (rest Seq.>< step cur)

basin :: Map.Map Point Int -> Point -> [Point]
basin grid = bfs (\v -> Seq.fromList $ filter
  (\v' -> case grid Map.!? v' of
    Nothing -> False
    Just n -> n /= 9 && n > (grid Map.! v)
  ) $ neighbours v)

neighbours :: Point -> [Point]
neighbours (x, y) = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
