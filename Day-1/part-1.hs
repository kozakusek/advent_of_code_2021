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
solution (h:t) = snd $ foldl f (h,0) t
  where
    f = (\(prev, inc) e  -> if e > prev then (e, inc + 1) else (e, inc))
  

