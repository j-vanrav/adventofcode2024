{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Function ((&))
import Data.List (foldl')

main = do
  content <- readFile "./day11/input"
  let stones = parseInput content
      part1 = length (applyN 25 expandStones stones)
  print part1

parseInput :: String -> [Int]
parseInput s = map read $ words s

expandStones :: [Int] -> [Int]
expandStones [] = []
expandStones [stone] =
  if
    | stone == 0 -> [1]
    | even (length d) -> (\(a, b) -> toInt a : [toInt b]) (splitHalf d)
    | otherwise -> [stone * 2024]
  where
    d = digits stone
expandStones (s : ss) = expandStones [s] ++ expandStones ss

digits :: Int -> [Int]
digits x = map (\c -> read [c]) (show x)

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

toInt :: [Int] -> Int
toInt [] = 0
toInt x = read $ concatMap show x

applyN :: Int -> (b -> b) -> b -> b
applyN = (foldr (.) id .) . replicate
