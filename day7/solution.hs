module Main where

main = do
  content <- readFile "./day7/input"
  let problem = map parseLine (lines content)
      solvable = map (\(t, os) -> solve t 0 os) problem
      part1 = sum (zipWith (\l r -> if r then fst l else 0) problem solvable)
  print part1

solve :: Int -> Int -> [Int] -> Bool
solve target counter [] = target == counter
solve target counter (o : os) = do
  let _add = counter + o
      _mul = counter * o
  solve target _add os || solve target _mul os

parseLine :: [Char] -> (Int, [Int])
parseLine ln = (read (init (head ws)), map read (tail ws))
  where
    ws = words ln