module Main where

import Debug.Trace (trace)

main = do
  content <- readFile "./day7/input"
  let problem = map parseLine (lines content)
      solvable = map (\(t, os) -> solve t 0 os) problem
      solvableWithConcat = map (\(t, os) -> solveWithConcat t 0 os) problem
      part1 = sum (zipWith (\l r -> if r then fst l else 0) problem solvable)
      part2 = sum (zipWith (\l r -> if r then fst l else 0) problem solvableWithConcat)
  print (part1, part2)

solve :: Int -> Int -> [Int] -> Bool
solve target counter [] = target == counter
solve target counter (o : os) = do
  let _add = counter + o
      _mul = counter * o
  solve target _add os || solve target _mul os

solveWithConcat :: Int -> Int -> [Int] -> Bool
solveWithConcat target counter [] = target == counter
solveWithConcat target counter (o : os) =
  (counter <= target)
    && ( do
           let _add = counter + o
               _mul = counter * o
               _concat = read (show counter ++ show o)
           solveWithConcat target _add os || solveWithConcat target _mul os || solveWithConcat target _concat os
       )

parseLine :: [Char] -> (Int, [Int])
parseLine ln = (read (init (head ws)), map read (tail ws))
  where
    ws = words ln