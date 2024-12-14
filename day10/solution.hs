module Main where

import Data.Function ((&))
import Data.Map qualified as M
import Data.Set qualified as S

main = do
  content <- readFile "./day10/input"
  let _grid = readToGrid M.empty (0, 0) content
      part1 = getGridScore _grid (M.toList _grid)
      part2 = getRating _grid (M.toList _grid)
  print (part1, part2)

type Grid = M.Map Coord Int

type GridList = [(Coord, Int)]

type Coord = (Int, Int)

readToGrid :: Grid -> Coord -> [Char] -> Grid
readToGrid mp (x, y) [c] = M.insert (x, y) (read [c]) mp
readToGrid mp (x, y) ('\n' : cs) = readToGrid mp (x + 1, 0) cs
readToGrid mp (x, y) (c : cs) = readToGrid (M.insert (x, y) (read [c]) mp) (x, y + 1) cs

getGridScore :: Grid -> GridList -> Int
getGridScore grid [] = 0
getGridScore grid (c : cs) = count + getGridScore grid cs
  where
    v = if snd c == 0 then getCellPeaks grid S.empty S.empty (fst c) else S.empty
    count = S.toList v & length

getCellPeaks :: Grid -> S.Set Coord -> S.Set Coord -> Coord -> S.Set Coord
getCellPeaks grid alreadyCounted alreadySearched start = if value == 9 then S.insert start alreadyCounted else searches
  where
    value = grid M.! start
    nc = move start 'N'
    sc = move start 'S'
    ec = move start 'E'
    wc = move start 'W'
    nv = M.lookup nc grid
    sv = M.lookup sc grid
    ev = M.lookup ec grid
    wv = M.lookup wc grid
    north = if maybe False (\x -> x == value + 1) nv then getCellPeaks grid alreadyCounted (S.insert start alreadySearched) nc else S.empty
    south = if maybe False (\x -> x == value + 1) sv then getCellPeaks grid alreadyCounted (S.insert start alreadySearched) sc else S.empty
    east = if maybe False (\x -> x == value + 1) ev then getCellPeaks grid alreadyCounted (S.insert start alreadySearched) ec else S.empty
    west = if maybe False (\x -> x == value + 1) wv then getCellPeaks grid alreadyCounted (S.insert start alreadySearched) wc else S.empty
    searches = S.empty & S.union north & S.union south & S.union east & S.union west

type Path = S.Set Coord

getRating :: Grid -> GridList -> Int
getRating grid [] = 0
getRating grid (c : cs) = v + getRating grid cs
  where
    v = if snd c == 0 then getCellPaths grid S.empty (fst c) else 0

getCellPaths :: Grid -> S.Set Coord -> Coord -> Int
getCellPaths grid alreadySearched start = if value == 9 then 1 else searches
  where
    value = grid M.! start
    nc = move start 'N'
    sc = move start 'S'
    ec = move start 'E'
    wc = move start 'W'
    nv = M.lookup nc grid
    sv = M.lookup sc grid
    ev = M.lookup ec grid
    wv = M.lookup wc grid
    north = if maybe False (\x -> x == value + 1) nv then getCellPaths grid (S.insert start alreadySearched) nc else 0
    south = if maybe False (\x -> x == value + 1) sv then getCellPaths grid (S.insert start alreadySearched) sc else 0
    east = if maybe False (\x -> x == value + 1) ev then getCellPaths grid (S.insert start alreadySearched) ec else 0
    west = if maybe False (\x -> x == value + 1) wv then getCellPaths grid (S.insert start alreadySearched) wc else 0
    searches = sum [north, south, east, west]

getAdjacentCoods :: Coord -> [Coord]
getAdjacentCoods coord = [move coord 'N', move coord 'S', move coord 'E', move coord 'W']

move (x, y) 'N' = (x - 1, y)
move (x, y) 'E' = (x, y + 1)
move (x, y) 'S' = (x + 1, y)
move (x, y) 'W' = (x, y - 1)
