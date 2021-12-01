import System.IO

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ (fmap iread) (words input)
  
iread :: String -> Int
iread = read

solution :: [Int] -> Int
solution (h:h':t) 
  | h' > h = 1 + solution (h':t)
  | otherwise = 0 + solution (h':t)

solution _ = 0
