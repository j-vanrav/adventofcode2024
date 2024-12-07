{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List (find)
import Data.Map qualified as M (Map, adjust, empty, filter, fromList, insert, lookup, mapWithKey, member, notMember, toList, (!))
import Data.Maybe (fromMaybe)

main = do
  content <- readFile "./day6/input"
  let mp = readGrid M.empty (0, 0) content
      soln = solve mp (findStart mp) 'N'
  print (writeGrid $ M.toList soln, countPath soln)

fib n = fibs !! n
  where
    fibs = 0 : 1 : zipWith (+) fibs (drop 1 fibs)

-- grid :: (Int,Int) -> (Int,Int,Int,Int)
-- grid (x,y) = g
--   where g = fromList []

-- g :: M.Map (Int, Int) Char -> (Int,Int) -> Bool
-- g mp (x, y) = maybe False _fst (M.lookup (x,y) (grid mp))

-- grid :: M.Map (Int, Int) Char -> M.Map (Int, Int) (Bool, Bool, Bool, Bool)
-- grid mp = do
--     let n = if lookup (x, y - 1) mp then  _fst (lookup (x, y - 1) grid)
--     fromList [((x, y), (, _snd (g (x, y - 1)), _fst (g (x, y - 1)), _fst (g (x, y - 1))))]

solve :: M.Map (Int, Int) Char -> (Int, Int) -> Char -> M.Map (Int, Int) Char
solve mp (x, y) d =
  if
    | M.notMember (move (x, y) d) mp -> M.insert (x, y) 'X' mp
    -- \| mp M.! move (x, y) d == 'X' -> M.insert (x, y) 'X' mp
    | mp M.! move (x, y) d == '#' -> solve mp (x, y) (rotate d)
    | True -> solve (M.insert (x, y) 'X' mp) (move (x, y) d) d

readGrid :: M.Map (Int, Int) Char -> (Int, Int) -> [Char] -> M.Map (Int, Int) Char
readGrid mp (x, y) [c] = M.insert (x, y) c mp
readGrid mp (x, y) ('\n' : cs) = readGrid mp (x + 1, 0) cs
readGrid mp (x, y) (c : cs) = readGrid (M.insert (x, y) c mp) (x, y + 1) cs

findStart :: M.Map (Int, Int) Char -> (Int, Int)
findStart mp = fst $ head $ M.toList $ M.filter (== '^') mp

countPath :: M.Map (Int, Int) Char -> Int
countPath mp = length $ M.toList $ M.filter (== 'X') mp

writeGrid :: [((Int, Int), Char)] -> String
writeGrid [((x, y), c)] = [c]
writeGrid (((x, y), c) : ((x_, y_), c_) : rest) = [c] ++ (if x /= x_ then "\n" else []) ++ writeGrid (((x_, y_), c_) : rest)

_fst (x, _, _, _) = x

_snd (_, x, _, _) = x

_thd (_, _, x, _) = x

_frt (_, _, _, x) = x

rotate 'N' = 'E'
rotate 'E' = 'S'
rotate 'S' = 'W'
rotate 'W' = 'N'

move (x, y) 'N' = (x - 1, y)
move (x, y) 'E' = (x, y + 1)
move (x, y) 'S' = (x + 1, y)
move (x, y) 'W' = (x, y - 1)