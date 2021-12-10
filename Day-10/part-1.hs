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
solution = sum . map (maybe 0 (maybe 0 score . snd) . getError)

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

score :: Char -> Int
score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
