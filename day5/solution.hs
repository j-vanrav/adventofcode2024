module Main where

import Data.Bifunctor (bimap)
import Data.List (find, sortBy)
import Data.List.Split (splitOn)
import Data.Map (Map, adjust, fromList, insert, member)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)

main = do
  content <- readFile "./day5/input"
  let (_rs, _ps) = rulesAndPages . words $ content
      (rs, ps) = (map rules _rs, map pages _ps)
  print (sumMiddles (ordered rs ps))
  print (sumMiddles (mapM (\uo -> makeOrdered rs uo []) (unordered rs ps)))

unordered :: [(Int, Int)] -> [[Int]] -> [[Int]]
unordered rs = filter (\p -> not (isOrdered p rs))

ordered :: [(Int, Int)] -> [[Int]] -> [[Int]]
ordered rs = filter (`isOrdered` rs)

toMap :: (Ord k) => k -> a -> Map k a
toMap i v = fromList [(i, v)]

rulesMap :: [(Int, Int)] -> Map Int ([Int], [Int]) -> Map Int ([Int], [Int])
rulesMap [(r1, r2)] mp = do
  let mp1 = if member r1 mp then adjust (\(l, r) -> (l, r ++ [r2])) r1 mp else insert r1 ([], [r2]) mp
  let mp2 = if member r2 mp1 then adjust (\(l, r) -> (l ++ [r1], r)) r2 mp1 else insert r2 ([r1], []) mp1
  mp2
rulesMap ((r1, r2) : rs) mp = do
  let mp1 = rulesMap [(r1, r2)] mp
  let mp2 = rulesMap rs mp1
  mp2

makeOrdered :: [(Int, Int)] -> [Int] -> [Int] -> [Int]
makeOrdered _ [] pgs = traceVal pgs
makeOrdered rs pgsr pgs = makeOrdered rs (tail pgsr) (placeInOrder rs [] (head pgsr) pgs)

placeInOrder :: [(Int, Int)] -> [Int] -> Int -> [Int] -> [Int]
placeInOrder rs lpgs p rpgs = if isOrdered (lpgs ++ [p] ++ rpgs) rs then lpgs ++ [p] ++ rpgs else placeInOrder rs (lpgs ++ [head rpgs]) p (tail rpgs)

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