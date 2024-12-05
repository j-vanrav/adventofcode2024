module Main where

import Data.Bifunctor (bimap)
import Data.List (find, sortBy)
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

main = do
  content <- readFile "./day5/input"
  let (_rs, _ps) = rulesAndPages . words $ content
      (rs, ps) = (map rules _rs, map pages _ps)
  print (sumMiddles (ordered rs ps))
  print (sumMiddles (fromMaybe [] (mapM (\uo -> getReorder rs uo []) (unordered rs ps))))

unordered :: [(Int, Int)] -> [[Int]] -> [[Int]]
unordered rs = filter (\p -> not (isOrdered p rs))

ordered :: [(Int, Int)] -> [[Int]] -> [[Int]]
ordered rs = filter (`isOrdered` rs)

sumMiddles :: [[Int]] -> Int
sumMiddles x = sum (map middle x)

getReorder :: [(Int, Int)] -> [Int] -> [Int] -> Maybe [Int]
getReorder rs psr ps = find (`isOrdered` rs) (allPermutations [] psr)

rulesAndPages :: [String] -> ([String], [String])
rulesAndPages [x] = if '|' `elem` x then ([x], []) else ([], [x])
rulesAndPages (x : xs) = do
  let (rs, ps) = rulesAndPages [x]
  bimap (rs ++) (ps ++) (rulesAndPages xs)

rules :: String -> (Int, Int)
rules x = do
  let chars = splitOn "|" x
  (read (head chars), read (last chars))

pages :: String -> [Int]
pages x = map read (splitOn "," x)

isOrdered :: [Int] -> [(Int, Int)] -> Bool
isOrdered [x, y] rs = checkRules (x, y) rs
isOrdered (x : ys) rs = all (\y -> checkRules (x, y) rs) ys && isOrdered ys rs
isOrdered _ _ = True

checkRules :: (Int, Int) -> [(Int, Int)] -> Bool
checkRules (x, y) [(a, b)] = (x /= b) || (y /= a)
checkRules (x, y) ((a, b) : abs) = checkRules (x, y) [(a, b)] && checkRules (x, y) abs

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l

middle :: [a] -> a
middle x = last (fst (splitHalf x))

allInserts :: [a] -> a -> [a] -> [[a]]
allInserts xs y [] = [xs ++ [y]]
allInserts xs y zs = (xs ++ [y] ++ zs) : allInserts (xs ++ [head zs]) y (tail zs)

allPermutations :: (Eq a, Show a) => [a] -> [a] -> [[a]]
allPermutations xs [] = [xs]
allPermutations xs ys = foldMap (\y -> allPermutations (xs ++ [y]) (delete y ys)) ys

delete :: (Eq a) => a -> [a] -> [a]
delete deleted xs = [x | x <- xs, x /= deleted]

traceVal :: (Show a) => a -> a
traceVal a = trace ("Trace " ++ show a) a