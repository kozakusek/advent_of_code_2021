import System.IO

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ (fmap iread) (words input)
  
iread :: String -> Int
iread = read

solution :: [Int] -> Int
solution [] = 0
solution (h:[]) = 0
solution (h:h':[]) = 0
solution (h:h':h'':[]) = 0
solution (h:h':h'':t) = fst $ foldl f (0, (h, h', h'')) t
  where
    f :: (Int, (Int, Int, Int)) -> Int -> (Int, (Int, Int, Int))
    f = (\(inc, (p1, p2, p3)) e -> 
      if e > p1 then (inc + 1, (p2, p3, e)) 
      else (inc, (p2, p3, e)))
  

