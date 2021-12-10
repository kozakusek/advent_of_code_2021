import System.IO
import Data.List.Split (splitOn)
import Data.List ( (\\), intersect, sort, union )

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution (splitOn "\n" input)

iread :: String -> Int
iread = read

solution :: [String] -> Int
solution = foldl (\a e -> a + solve e) 0

solve :: String -> Int
solve line = length $ filter pred $ decode ([], [], [], [], [], [], []) "" inp out
  where
    pred :: Int -> Bool
    pred x = x == 1 || x == 4 || x == 7 || x == 8
    inp = words $ head $ splitOn "|" line
    out = words $ head $ tail $ splitOn "|" line

decode :: (String, String, String, String, String, String, String) -> String
  -> [String] -> [String] -> [Int]
decode ([a], [b], [c], [d], [e], [f], [g]) _ _ out = map (digit . remap) out
  where
    remap :: String -> String
    remap str = sort $ map fun str
    fun :: Char -> Char
    fun 'a' = a
    fun 'b' = b
    fun 'c' = c
    fun 'd' = d
    fun 'e' = e
    fun 'f' = f
    fun 'g' = g
    fun  _  = ' '
decode (a, b, c, d, e, f, g) used (inh:int) out = decode (a'', b'', c'', d'', e'', f'', g'') used' int out
  where
    n = length inh
    letters = getLetters n inh
    a' = if 'a' `elem` letters then myintersect a $ getSet n else a
    b' = if 'b' `elem` letters then myintersect b $ getSet n else b
    c' = if 'c' `elem` letters then myintersect c $ getSet n else c
    d' = if 'd' `elem` letters then myintersect d $ getSet n else d
    e' = if 'e' `elem` letters then myintersect e $ getSet n else e
    f' = if 'f' `elem` letters then myintersect f $ getSet n else f
    g' = if 'g' `elem` letters then myintersect g $ getSet n else g
    (used', [a'', b'', c'', d'', e'', f'', g''])= updateUsed used [a', b', c', d', e', f', g'] 7

decode _ _ _ _ = []

removeUsed :: String -> String -> String
removeUsed _ [a] = [a]
removeUsed used a = a \\ used

updateUsed :: String -> [String] -> Int-> (String, [String])
updateUsed a b 0 = (a, b)
updateUsed used str n = updateUsed used' (map (removeUsed used') str) (n-1)
  where
    used' = foldl f used str
    f :: String -> String -> String
    f u [a] = u `union` [a]
    f u _ = u 

myintersect :: Eq a => [a] -> [a] -> [a]
myintersect [] lst = lst
myintersect l1 l2 = l1 `intersect` l2

digit :: String -> Int
digit "abcefg"  = 0
digit "cf"      = 1
digit "acdeg"   = 2
digit "acdfg"   = 3
digit "bcdf"    = 4
digit "abdfg"   = 5
digit "abdefg"  = 6
digit "acf"     = 7
digit "abcdefg" = 8
digit "abcdfg"  = 9
digit _ = -1

getSet :: Int -> String
getSet 2 = ['c', 'f']
getSet 3 = ['a', 'c', 'f']
getSet 4 = ['a', 'e', 'g']
getSet 5 = ['b', 'c', 'e', 'f']
getSet 6 = ['c', 'd', 'e']
getSet _ = []

getLetters :: Int -> String -> String
getLetters 2 a = a
getLetters 3 a = a
getLetters _ a = "abcdefg" \\ a
