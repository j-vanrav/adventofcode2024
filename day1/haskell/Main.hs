module Main where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import System.IO ()
import Text.Read (readMaybe)

main :: IO ()
main = do
  content <- readFile "../input"
  let linesOfFile = lines content
      parsedLines = fromMaybe [] (mapM parseLine linesOfFile)
      firstList = sort (map fst parsedLines)
      secondList = sort (map snd parsedLines)
      dist = calcDistance firstList secondList
      sim = calcSimilarity firstList secondList
  print (show dist ++ ", " ++ show sim)

calcDistance :: (Num a) => [a] -> [a] -> a
calcDistance a b = sum (zipWith (\x y -> abs (y - x)) a b)

calcSimilarity :: (Eq a, Num a) => [a] -> [a] -> a
calcSimilarity a b = sum (zipWithMulti (\x y -> y * numMatches x y) a b)

numMatches :: (Eq a, Num a) => [a] -> a -> a
numMatches [] _ = 0
numMatches [x] a = if x == a then 1 else 0
numMatches (x : xs) a = numMatches [x] a + numMatches xs a

parseLine :: String -> Maybe (Int, Int)
parseLine line =
  case splitOn "   " line of
    [left, right] -> do
      leftNum <- readMaybe (trim left) :: Maybe Int
      rightNum <- readMaybe (trim right) :: Maybe Int
      return (leftNum, rightNum)
    _ -> Nothing
  where
    trim = unwords . words

zipWithMulti :: ([a] -> b -> c) -> [a] -> [b] -> [c]
zipWithMulti f as = map (f as)

traceVal :: (Show a) => a -> a
traceVal a = trace ("Trace " ++ show a) a