module Main where

import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Debug.Trace (trace)
import System.IO ()
import Text.Read (readMaybe)

main :: IO ()
main = do
  content <- readFile "../input"
  let linesOfFile = lines content
      parsedLines = mapMaybe parseLine linesOfFile
      firstList = sort (map fst parsedLines)
      secondList = sort (map snd parsedLines)
      dist = calcDistance firstList secondList
      sim = calcSimilarity firstList secondList
  putStrLn (show dist ++ ", " ++ show sim)

calcDistance :: (Num a) => [a] -> [a] -> a
calcDistance a b = sum (zipWith (\x y -> abs (y - x)) a b)

calcSimilarity :: [Int] -> [Int] -> Int
calcSimilarity l1 l2 = sum (zipWith (\x _ -> x * countMatches x l2) l1 l2)

countMatches :: (Eq a) => a -> [a] -> Int
countMatches a = length . filter (== a)

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

traceVal :: (Show a) => a -> a
traceVal a = trace ("Trace " ++ show a) a