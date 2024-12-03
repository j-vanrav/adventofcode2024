import Data.List (sort, sortBy)
import Data.Maybe (fromMaybe, mapMaybe)
import System.IO ()
import Text.Read (readMaybe)

main = do
  content <- readFile "./day2/input"
  let rows = filter (not . null) (fromMaybe [] (mapM ints (lines content)))
  print (length (filter id (map isSafe rows)))

isSafe :: [Int] -> Bool
isSafe x = isAdjacencySafe x && (isAscending x || isDescending x)

isAdjacencySafe :: [Int] -> Bool
isAdjacencySafe [x] = True
isAdjacencySafe (x : y) = isPairSafe (x, head y) && isAdjacencySafe y
isAdjacencySafe _ = True

isPairSafe :: (Int, Int) -> Bool
isPairSafe (x, y) = (x /= y) && (abs (y - x) <= 3)

ints :: String -> Maybe [Int]
ints = mapM readMaybe . words

isAscending :: [Int] -> Bool
isAscending x = sort x == x

isDescending :: [Int] -> Bool
isDescending x = sortBy (flip compare) x == x
