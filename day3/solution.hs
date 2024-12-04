module Main where

import Data.Foldable
import Data.List (sort, sortBy)
import Data.List.NonEmpty (sortWith)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Text.Regex.TDFA (getAllTextMatches, (=~))

main = do
  content <- readFile "./day3/input"
  let pattern = "mul\\((\\d+),(\\d+)\\)"
  print (parseMult 0 True (getMuls content))

parseMult :: Int -> Bool -> [String] -> Int
parseMult total _ (('d' : 'o' : '(' : _) : xs) = parseMult total True xs
parseMult total _ (('d' : 'o' : 'n' : _) : xs) = parseMult total False xs
parseMult total False (('m' : 'u' : 'l' : _) : xs) = parseMult total False xs
parseMult total True (('m' : 'u' : 'l' : x) : xs) = parseMult (mult (getNums x) + total) True xs
parseMult total _ _ = total

getMuls :: String -> [String]
getMuls c = do
  let x = getNext c
      hasMatch = not (null (_snd x))
  if hasMatch then _snd x : getMuls (_thd x) else []

getNums :: String -> [Int]
getNums c = map read (getAllTextMatches ((c :: String) =~ "[0-9]+") :: [String])

getNext :: String -> (String, String, String)
getNext c = do
  let mul = (c :: String) =~ "mul\\(([0-9]+),([0-9]+)\\)" :: (String, String, String)
      on = (c :: String) =~ "do\\(\\)" :: (String, String, String)
      off = (c :: String) =~ "don't\\(\\)" :: (String, String, String)
  minimumBy (\(a, _, _) (b, _, _) -> compare a b) [mul, on, off]

_fst :: (a, b, c) -> a
_fst (x, _, _) = x

_snd :: (a, b, c) -> b
_snd (_, y, _) = y

_thd :: (a, b, c) -> c
_thd (_, _, z) = z

mult :: [Int] -> Int
mult [x, y] = x * y
