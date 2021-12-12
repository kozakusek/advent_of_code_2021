import System.IO

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution input

solution :: String -> Int
solution = length . takeWhile (< 100) . zeros

zeros :: String -> [Int]
zeros = map (length . filter (== 0) . concat) . iterate step
  . (map . map) (read . (:[])) . lines

step :: [[Int]] -> [[Int]]
step = fix interactions . (map . map) succ

fix :: Eq a => (a -> a) -> (a -> a)
fix f = go
  where
    go x = case f x of x'
      | x' == x   -> x'
      | otherwise -> go x'

interactions :: [[Int]] -> [[Int]]
interactions grid =
  let l  = left  grid
    r  = right grid
    u  = up  grid
    d  = down  grid
    lu = left  u
    ld = left  d
    ru = right u
    rd = right d
  in  (zipWith . zipWith) flashed grid $ foldr ((zipWith . zipWith) interaction) grid
      [l, r, u, d, lu, ld, ru, rd]
  where
    left  = map (0 :)
    right = map ((++ [0]) . tail)
    up    = (repeat 0 :)
    down  = (++ [repeat 0]) . tail
    interaction _ 0 = 0
    interaction n m
      | n > 9   = succ m
      | otherwise =    m
    flashed n m
      | n > 9   = 0
      | otherwise = m
