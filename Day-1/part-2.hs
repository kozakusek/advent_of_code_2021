import System.IO

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ (fmap iread) (words input)
  
iread :: String -> Int
iread = read

solution :: [Int] -> Int
solution (h:h':h'':h''':t) 
  | h''' > h = 1 + solution (h':h'':h''':t)
  | otherwise = 0 + solution (h':h'':h''':t)

solution _ = 0
  

