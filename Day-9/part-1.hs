import System.IO
import Data.Char (digitToInt)
import Data.Maybe (mapMaybe)
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

solution :: Map.Map Point Int -> Int
solution grid = (sum $ Map.map (+ 1) $ 
    Map.filterWithKey (\v k -> all (> k) (mapMaybe (grid Map.!?) (neighbours v))) grid)

parse :: String -> Map.Map Point Int
parse = (\rows -> Map.fromList 
    [((r, c), digitToInt n) | (r, row) <- zip [0..] rows, (c, n) <- zip [0..] row]) . lines

bfs :: (Ord a, Eq a) => (a -> Seq.Seq a) -> a -> [a]
bfs step start = rec Set.empty (Seq.singleton start)
  where rec seen Seq.Empty = []
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
