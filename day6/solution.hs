{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.List (find)
import Data.Map qualified as M (Map, adjust, empty, filter, fromList, insert, lookup, mapWithKey, member, notMember, toList, update, (!))
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (trace)

main = do
  content <- readFile "./day6/sample"
  let mp = readGrid M.empty (0, 0) content
      start = findStart mp
      solution = solve mp start 'N'
      isSolvable = solvable mp start 'N' M.empty
  print (writeGrid $ M.toList solution, countPath solution)
  print (isSolvable)

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

solvable :: M.Map (Int, Int) Char -> (Int, Int) -> Char -> M.Map (Int, Int) (Maybe Bool, Maybe Bool, Maybe Bool, Maybe Bool) -> (Bool, M.Map (Int, Int) (Maybe Bool, Maybe Bool, Maybe Bool, Maybe Bool))
solvable mp (x, y) d soln =
  if
    | M.member (x, y) soln && solnHasD (soln M.! (x, y)) d -> (False, soln)
    | M.notMember (move (x, y) d) mp -> (True, insertOrUpdateSolnDir soln (x, y) d (Just True))
    -- \| mp M.! move (x, y) d == 'X' -> M.insert (x, y) 'X' mp
    | mp M.! move (x, y) d == '#' -> (solvable mp (x, y) (rotate d) (insertOrUpdateSolnDir soln (x, y) d (Just False)))
    | True -> (solvable (M.insert (x, y) 'X' mp) (move (x, y) d) d (insertOrUpdateSolnDir soln (x, y) d (Just True)))

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

_fst (n, _, _, _) = n

_snd (_, e, _, _) = e

_thd (_, _, s, _) = s

_frt (_, _, _, w) = w

rotate 'N' = 'E'
rotate 'E' = 'S'
rotate 'S' = 'W'
rotate 'W' = 'N'

move (x, y) 'N' = (x - 1, y)
move (x, y) 'E' = (x, y + 1)
move (x, y) 'S' = (x + 1, y)
move (x, y) 'W' = (x, y - 1)

solnDir s 'N' = _fst s
solnDir s 'E' = _snd s
solnDir s 'S' = _thd s
solnDir s 'W' = _frt s

setSolnDir (n, e, s, w) 'N' b = (b, e, s, w)
setSolnDir (n, e, s, w) 'E' b = (n, b, s, w)
setSolnDir (n, e, s, w) 'S' b = (n, e, b, w)
setSolnDir (n, e, s, w) 'W' b = (n, e, s, b)

solnHasD (n, e, s, w) 'N' = isJust n
solnHasD (n, e, s, w) 'E' = isJust e
solnHasD (n, e, s, w) 'S' = isJust s
solnHasD (n, e, s, w) 'W' = isJust w

esd = (Nothing, Nothing, Nothing, Nothing)

insertOrUpdateSolnDir :: M.Map (Int, Int) (Maybe Bool, Maybe Bool, Maybe Bool, Maybe Bool) -> (Int, Int) -> Char -> Maybe Bool -> M.Map (Int, Int) (Maybe Bool, Maybe Bool, Maybe Bool, Maybe Bool)
insertOrUpdateSolnDir soln (x, y) d b = if M.member (x, y) soln then M.update (\sd -> Just $ setSolnDir sd d b) (x, y) soln else M.insert (x, y) (setSolnDir esd d b) soln

traceVal :: (Show a) => a -> a
traceVal a = trace ("Trace " ++ show a) a