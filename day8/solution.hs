module Main where

import Data.Map qualified as M (Map, adjust, empty, filter, fromList, insert, lookup, mapWithKey, member, notMember, toList, update, (!))
import Data.Set qualified as S (fromList, toList)

main :: IO ()
main = do
  content <- readFile "./day8/input"
  let _initialGrid = readToGrid M.empty (0, 0) content
      _gridWithAntinodes = generateAntinodes content _initialGrid
      _nAntinodes = countAntinodes _gridWithAntinodes
  print _nAntinodes

readToGrid :: M.Map (Int, Int) (Char, Bool) -> (Int, Int) -> [Char] -> M.Map (Int, Int) (Char, Bool)
readToGrid mp (x, y) [c] = M.insert (x, y) (c, False) mp
readToGrid mp (x, y) ('\n' : cs) = readToGrid mp (x + 1, 0) cs
readToGrid mp (x, y) (c : cs) = readToGrid (M.insert (x, y) (c, False) mp) (x, y + 1) cs

generateAntinodes :: String -> M.Map (Int, Int) (Char, Bool) -> M.Map (Int, Int) (Char, Bool)
generateAntinodes gridString grid = do
  let _frequencies = getFrequencies gridString
      _towerss = map (getTowers grid) _frequencies
      _antiNodes = concatMap getAntinodes _towerss
      _newGrid = placeAntinodes grid _antiNodes
  _newGrid

getTowers :: M.Map (Int, Int) (Char, Bool) -> Char -> [(Int, Int)]
getTowers grid tower = map fst (M.toList (M.filter (\cell -> fst cell == tower) grid))

placeAntinodes :: M.Map (Int, Int) (Char, Bool) -> [(Int, Int)] -> M.Map (Int, Int) (Char, Bool)
placeAntinodes grid [] = grid
placeAntinodes grid [(x, y)] = M.adjust (\(c, b) -> (c, True)) (x, y) grid
placeAntinodes grid ((x, y) : ans) = placeAntinodes (placeAntinodes grid [(x, y)]) ans

getFrequencies :: [Char] -> [Char]
getFrequencies input = remove '.' (S.toList . S.fromList $ input)

getAntinodes :: [(Int, Int)] -> [(Int, Int)]
getAntinodes [] = []
getAntinodes [_] = []
getAntinodes [(a, b), (c, d)] = [(a - o1, b - o2), (c + o1, d + o2)]
  where
    (o1, o2) = (c - a, d - b)
getAntinodes ((a, b) : o) = concatMap (\(x, y) -> getAntinodes [(a, b), (x, y)]) o ++ getAntinodes o

countAntinodes :: M.Map (Int, Int) (Char, Bool) -> Int
countAntinodes grid = length (M.toList (M.filter snd grid))

remove :: (Eq a) => a -> [a] -> [a]
remove el = filter (/= el)
