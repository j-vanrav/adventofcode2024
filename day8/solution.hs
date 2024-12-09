module Main where

import Data.List (sortBy)
import Data.Map qualified as M (Map, adjust, empty, filter, fromList, insert, lookup, mapWithKey, member, notMember, toList, update, (!))
import Data.Set qualified as S (fromList, toList)
import GHC.Base (maxInt)

main :: IO ()
main = do
  content <- readFile "./day8/sample"
  let _initialGrid = readToGrid M.empty (0, 0) content
      (_gridWAntinodes, _gridWHarmonics) = generateAntinodes content _initialGrid
      _part1 = countAntinodes _gridWAntinodes
      _part2 = countAntinodes _gridWHarmonics

  print (_part1, _part2)
  mapM_ putStrLn [writeGrid "" (M.toList _gridWHarmonics)]

writeGrid :: String -> [((Int, Int), (Char, Bool))] -> String
writeGrid s [] = s
writeGrid s lgrid
  | _isNextNewline = writeGrid (s ++ ['\n']) lgrid
  | _isNextAntinode = writeGrid (s ++ ['#']) _remainder
  | otherwise = writeGrid (s ++ [_nextChar]) _remainder
  where
    _next = head lgrid
    _remainder = tail lgrid
    _nLine = fst (fst _next)
    _isNextNewline = _nLine > countChar '\n' s
    _nextChar = fst (snd _next)
    _isNextAntinode = snd (snd _next) && _nextChar == '.'

readToGrid :: M.Map (Int, Int) (Char, Bool) -> (Int, Int) -> [Char] -> M.Map (Int, Int) (Char, Bool)
readToGrid mp (x, y) [c] = M.insert (x, y) (c, False) mp
readToGrid mp (x, y) ('\n' : cs) = readToGrid mp (x + 1, 0) cs
readToGrid mp (x, y) (c : cs) = readToGrid (M.insert (x, y) (c, False) mp) (x, y + 1) cs

getGridBounds :: M.Map (Int, Int) (Char, Bool) -> (Int, Int)
getGridBounds grid = fst $ last $ M.toList grid

generateAntinodes :: String -> M.Map (Int, Int) (Char, Bool) -> (M.Map (Int, Int) (Char, Bool), M.Map (Int, Int) (Char, Bool))
generateAntinodes gridString grid = (_gridWAntinodes, _gridWHarmonics)
  where
    _frequencies = getFrequencies gridString
    _towerss = map (getTowers grid) _frequencies
    _antiNodes = concatMap getAntinodes _towerss
    _bounds = getGridBounds grid
    _harmonicAntiNodes = concatMap (getHarmonicAntinodes _bounds) _towerss
    _gridWAntinodes = placeAntinodes grid _antiNodes
    _gridWHarmonics = placeAntinodes grid _harmonicAntiNodes

getTowers :: M.Map (Int, Int) (Char, Bool) -> Char -> [(Int, Int)]
getTowers grid tower = map fst (M.toList (M.filter (\cell -> fst cell == tower) grid))

placeAntinodes :: M.Map (Int, Int) (Char, Bool) -> [(Int, Int)] -> M.Map (Int, Int) (Char, Bool)
placeAntinodes grid [] = grid
placeAntinodes grid [(x, y)] = M.adjust (\(c, b) -> (c, True)) (x, y) grid
placeAntinodes grid ((x, y) : ans) = placeAntinodes (placeAntinodes grid [(x, y)]) ans

getFrequencies :: [Char] -> [Char]
getFrequencies input = remove '\n' $ remove '.' (S.toList . S.fromList $ input)

countChar :: Char -> [Char] -> Int
countChar c cs = length $ filter (== c) cs

getAntinodes :: [(Int, Int)] -> [(Int, Int)]
getAntinodes [] = []
getAntinodes [_] = []
getAntinodes [(a, b), (c, d)] = [(a - o1, b - o2), (c + o1, d + o2)]
  where
    (o1, o2) = (c - a, d - b)
getAntinodes ((a, b) : o) = concatMap (\(x, y) -> getAntinodes [(a, b), (x, y)]) o ++ getAntinodes o

getHarmonicAntinodes :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
getHarmonicAntinodes bounds [] = []
getHarmonicAntinodes (bx, by) [(a, b)] = []
getHarmonicAntinodes (bx, by) [(a, b), (c, d)] = continueAntinodes (0, 0) (-sx, -sy) (a, b) ++ continueAntinodes (bx, by) (sx, sy) (a, b)
  where
    (sx, sy) = (c - a, d - b)
getHarmonicAntinodes bounds ((a, b) : o) = concatMap (\(x, y) -> getHarmonicAntinodes bounds [(a, b), (x, y)]) o ++ getHarmonicAntinodes bounds o

continueAntinodes :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
continueAntinodes (bx, by) (sx, sy) (x, y) =
  [ (x + (i * sx), y + (i * sy)) | i <- [0 .. safeLimit]
  ]
  where
    limX = if sx < 0 then 0 else bx
    limY = if sy < 0 then 0 else by
    _maxXSteps = abs (if sx == 0 then maxInt else (limX - x) `div` sx)
    _maxYSteps = abs (if sy == 0 then maxInt else (limY - y) `div` sy)
    limit = min _maxXSteps _maxYSteps
    safeLimit = if limit == maxInt then 0 else limit

countAntinodes :: M.Map (Int, Int) (Char, Bool) -> Int
countAntinodes grid = length (M.toList (M.filter snd grid))

remove :: (Eq a) => a -> [a] -> [a]
remove el = filter (/= el)
