module Main where

import Data.List (sort, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import System.IO ()
import Text.Read (readMaybe)

main = do
  content <- readFile "./day2/input"
  let rows = filter (not . null) (fromMaybe [] (mapM ints (lines content)))
  print (length (filter id (map isSafe rows)))
  print (length (filter id (map isSafeWithDampening rows)))

isSafe :: [Int] -> Bool
isSafe x = isAdjacencySafe x 0

isSafeWithDampening :: [Int] -> Bool
isSafeWithDampening x = isAdjacencySafeWithDampening x 0

isAdjacencySafe :: [Int] -> Int -> Bool
isAdjacencySafe [] _ = True
isAdjacencySafe [x] _ = True
isAdjacencySafe [x, y] _ = isPairSafe (x, y)
isAdjacencySafe [x, y, z] _ = isTrebleSafe (x, y, z)
isAdjacencySafe (x : y : z) i = maybe True isTrebleSafe (toTreble (take 3 (drop i (x : y : z)))) && (((i + 3) > length (x : y : z)) || isAdjacencySafe (x : y : z) (i + 1))

isAdjacencySafeWithDampening :: [Int] -> Int -> Bool
isAdjacencySafeWithDampening [] _ = True
isAdjacencySafeWithDampening [x] _ = True
isAdjacencySafeWithDampening [x, y] _ = isPairSafe (x, y) || isDampenableAt [x, y] 0
isAdjacencySafeWithDampening [x, y, z] _ = (isTrebleSafe (x, y, z) && (isAscending [x, y, z] || isDescending [x, y, z])) || isDampenableAt [x, y, z] 1
isAdjacencySafeWithDampening (x : y : z) i = (maybe True isTrebleSafe (toTreble (take 3 (drop i (x : y : z)))) || isDampenableAt (x : y : z) (i + 1)) && (((i + 3) > length (x : y : z)) || isAdjacencySafeWithDampening (x : y : z) (i + 1))

isPairSafe :: (Int, Int) -> Bool
isPairSafe (x, y) = (x /= y) && (abs (y - x) <= 3)

toTreble :: [Int] -> Maybe (Int, Int, Int)
toTreble [x, y, z] = Just (x, y, z)
toTreble _ = Nothing

isTrebleSafe :: (Int, Int, Int) -> Bool
isTrebleSafe (x, y, z) = isPairSafe (x, y) && isPairSafe (y, z) && (isAscending [x, y, z] || isDescending [x, y, z])

isDampenableAt :: [Int] -> Int -> Bool
isDampenableAt as i = any isSafe [deleteAt as (i - 1), deleteAt as i, deleteAt as (i + 1)]

ints :: String -> Maybe [Int]
ints = mapM readMaybe . words

isAscending :: [Int] -> Bool
isAscending x = sort x == x

isDescending :: [Int] -> Bool
isDescending x = sortBy (flip compare) x == x

deleteAt :: [Int] -> Int -> [Int]
deleteAt xs y = take y xs ++ drop 1 (drop y xs)