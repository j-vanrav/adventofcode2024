module Main where

import Data.List (sort)
import Data.Maybe (mapMaybe)
import System.IO ()
import Text.Read (readMaybe)

main :: IO ()
main = do
  content <- readFile "../input"
  let rows = parseFile content
      leftCol = sort (map fst rows)
      rightCol = sort (map snd rows)
      dist = calcDistance leftCol rightCol
      sim = calcSimilarity leftCol rightCol
  putStrLn (show dist ++ ", " ++ show sim)

calcDistance :: (Num a) => [a] -> [a] -> a
calcDistance a b = sum (zipWith (\x y -> abs (y - x)) a b)

calcSimilarity :: [Int] -> [Int] -> Int
calcSimilarity l1 l2 = sum (zipWith (\x _ -> x * countMatches x l2) l1 l2)

countMatches :: (Eq a) => a -> [a] -> Int
countMatches a = length . filter (== a)

parseFile :: String -> [(Int, Int)]
parseFile = mapMaybe readTwoInts . lines

readTwoInts :: String -> Maybe (Int, Int)
readTwoInts = parseTwo . mapMaybe readMaybe . words

parseTwo :: [a] -> Maybe (a, a)
parseTwo [x, y] = Just (x, y)
parseTwo _ = Nothing