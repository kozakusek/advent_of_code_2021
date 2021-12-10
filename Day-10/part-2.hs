{-# OPTIONS -Wno-incomplete-patterns #-}
import System.IO
import Text.Parsec as Parsec (ParseError, char, between, choice, eof, many, parse, ParsecT)
import Text.Parsec.Error (errorMessages, messageString)
import Control.Monad (void)
import Control.Applicative (liftA2)
import Data.List (find, sort)
import Data.Maybe (isNothing)

main :: IO ()
main = do
  h <- openFile "input.txt" ReadMode
  input <- hGetContents h
  print $ solution $ (lines input)

solution :: [String] -> Int
solution = ((!!) <*> (`div` 2) . length) 
  . sort . map (foldl ((+) . (* 5)) 0 . scores) . filter (maybe False (isNothing . snd) . getError)

chunks :: Monad m => ParsecT String u m ()
chunks = void (many chunk)
  where chunk = choice
          [ between (char '(') (char ')') chunks
          , between (char '[') (char ']') chunks
          , between (char '{') (char '}') chunks
          , between (char '<') (char '>') chunks
          ]

getError :: String -> Maybe (Maybe Char, Maybe Char)
getError = either (Just . liftA2 (,) last head . map (find (/= '"') . messageString)
  . errorMessages) (const Nothing) . Parsec.parse (chunks >> eof) ""

scores :: String -> [Int]
scores x = maybe [] (maybe [] (\c -> score c : scores (x ++ [c])) . fst) (getError x)

score :: Char -> Int
score ')' = 1
score ']' = 2
score '}' = 3
score '>' = 4