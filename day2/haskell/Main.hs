import Data.List (sort, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import System.IO ()
import Text.Read (readMaybe)

main = do
  content <- readFile "./day2/input"
  let rows = filter (not . null) (fromMaybe [] (mapM ints (lines content)))
  print (length (filter id (map isSafe rows)))

isSafe :: [Int] -> Bool
isSafe x = isAdjacencySafe x 0 && (isAscending x || isDescending x)

isAdjacencySafe :: [Int] -> Int -> Bool
isAdjacencySafe [] _ = True
isAdjacencySafe [x] _ = True
isAdjacencySafe [x, y] _ = isPairSafe (x, y)
isAdjacencySafe [x, y, z] _ = isTrebleSafe (x, y, z)
isAdjacencySafe (x : y : z) i = isAdjacencySafe (take 3 (drop i (x : y : z))) 0 && (((i + 3) > length (x : y : z)) || isAdjacencySafe (x : y : z) (i + 1))

isPairSafe :: (Int, Int) -> Bool
isPairSafe (x, y) = (x /= y) && (abs (y - x) <= 3)

isTrebleSafe :: (Int, Int, Int) -> Bool
isTrebleSafe (x, y, z) = isPairSafe (x, y) && isPairSafe (y, z) && (isAscending [x, y, z] || isDescending [x, y, z])

ints :: String -> Maybe [Int]
ints = mapM readMaybe . words

isAscending :: [Int] -> Bool
isAscending x = sort x == x

isDescending :: [Int] -> Bool
isDescending x = sortBy (flip compare) x == x

deleteAt :: [Int] -> Int -> [Int]
deleteAt xs y = take y xs ++ drop 1 (drop y xs)