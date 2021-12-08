import System.IO
import Data.List.Split (splitOn)
import GHC.Float (int2Double)
import Data.List (sort)

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ (splitOn '\n' input)

iread :: String -> Int
iread = read

solution :: [String] -> Int
solution lst = foldl (\a e -> a + solve e) 0 lst
  
solve :: String -> Int
solve line = length $ filter pred $ decode ([], [], [], [], [], [], []) inp out
  where
    pred :: Int -> Bool
    pred x = if x == 1 || x == 4 || x == 7 || x == 8 then True else False
    inp = words $ head $ splitOn '|' line
    out = words $ tail $ splitOn '|' line

decode :: ([Char], [Char], [Char], [Char], [Char], [Char], [Char])
  -> [String] -> [String] -> [Int]
decode (a:[], b:[], c:[], d:[], e:[], f:[], g:[]) _ out = map (digit . remap) out
  where
    remap :: String -> String
    remap str = sort $ map f str
    f :: Char -> Char
    f 'a' = a
    f 'b' = b
    f 'c' = c
    f 'd' = d
    f 'e' = e
    f 'f' = f
    f 'g' = g
decode (a, b, c, d, e, f, g) (inh:int) out = decode (a', b', c', d', e', f', g') int out
  where
    n = length inh
    letters = getLetters n inh
    a' = if elem 'a' letters then intersect a $ getSet n else a
    b' = if elem 'b' letters then intersect b $ getSet n else b
    c' = if elem 'c' letters then intersect c $ getSet n else c
    d' = if elem 'd' letters then intersect d $ getSet n else d
    e' = if elem 'e' letters then intersect e $ getSet n else e
    f' = if elem 'f' letters then intersect f $ getSet n else f
    g' = if elem 'g' letters then intersect g $ getSet n else g


digit :: String -> Int
digit "abcefg" = 0
digit "cf"     = 1
digit "acdeg"  = 2
digit "acdfg"  = 3
digit "bcdf"   = 4
digit "abdfg"  = 5
digit "abdefg" = 6
digit "acf"    = 7
digit _        = 8
digit "abcdfg" = 9

getSet :: Int -> [Char]
getSet 2 = ['c', 'f']
getSet 3 = ['a', 'c', 'f']
getSet 4 = ['a', 'e', 'g']
getSet 5 = ['b', 'c', 'e', 'f']
getSet 6 = ['c', 'd', 'e']

getLetters :: Int -> String -> [Char]
getLetters 2 a = a
getLetters 3 a = a
getLetters _ a = "abcdefg" \\ a
