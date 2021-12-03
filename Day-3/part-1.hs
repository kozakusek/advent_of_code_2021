import System.IO

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution (words input)

solution :: [String] -> Int
solution (h:t) = gamma * eps
	where
		gamma = bintodec $ map (\x -> if x > 0 then 1 else 0) common
		eps = bintodec $ map (\x -> if x > 0 then 0 else 1) common
		common = foldr f acc0 t
			where
				acc0 = digits h
				f el acc = zipWith (+) (digits el) acc 
		
digits :: String -> [Int]
digits (h:t) 
	| h == '0' = (-1):(digits t)
	| otherwise = 1:(digits t)
digits [] = []

bintodec :: [Int] -> Int
bintodec lst = foldl (\a e -> 2*a + e) 0 lst