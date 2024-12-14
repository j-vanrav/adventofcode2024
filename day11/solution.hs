module Main where

import Data.Function ((&))
import Data.List (foldl')
import Data.Map qualified as M

main = do
  content <- readFile "./day11/input"
  let stones = toStones 1 $ parseInput content
      part1 = countStones $ applyN 25 expandStones stones
      part2 = countStones $ applyN 75 expandStones stones
  print (part1, part2)

type Stones = M.Map Int Int

parseInput :: String -> [Int]
parseInput s = map read $ words s

expandStones :: Stones -> Stones
expandStones = M.foldrWithKey (\k v acc -> M.unionWith (+) (expandStone v k) acc) M.empty

expandStone :: Int -> Int -> Stones
expandStone n stone
  | stone == 0 = M.fromList [(1, n)]
  | even (length d) = toStones n $ (\(a, b) -> toInt a : [toInt b]) (splitHalf d)
  | otherwise = M.fromList [(stone * 2024, n)]
  where
    d = digits stone

toStones :: Int -> [Int] -> Stones
toStones n = foldl (\acc v -> M.insertWith (+) v n acc) M.empty

countStones :: Stones -> Int
countStones = foldl' (+) 0

digits :: Int -> [Int]
digits x = map (\c -> read [c]) (show x)

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

toInt :: [Int] -> Int
toInt [] = 0
toInt x = read $ concatMap show x

applyN :: Int -> (b -> b) -> b -> b
applyN = (foldr (.) id .) . replicate