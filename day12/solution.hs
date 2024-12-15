module Main where

import Data.List (foldl')
import Data.Map qualified as M
import Data.Maybe (isNothing)
import Data.Set qualified as S
import Debug.Trace (trace)

main = do
  content <- readFile "./day12/input"
  let grid = readToGrid M.empty (0, 0) content
      garden = toGarden grid
      cropRegions = getRegions grid
      part1 = getPrice garden cropRegions
      part2 = getPriceBulkDiscount garden cropRegions
  print (part1, part2)

type Coord = (Int, Int)

type NFences = Int

type NCorners = Int

type Crop = Char

type Plot = (Crop, NFences, NCorners)

type Garden = M.Map Coord Plot

type Grid = M.Map Coord Crop

type Region = S.Set Coord

type Regions = [Region]

type CropRegions = M.Map Crop Regions

readToGrid :: Grid -> Coord -> [Char] -> Grid
readToGrid mp (x, y) [c] = M.insert (x, y) c mp
readToGrid mp (x, y) ('\n' : cs) = readToGrid mp (x + 1, 0) cs
readToGrid mp (x, y) (c : cs) = readToGrid (M.insert (x, y) c mp) (x, y + 1) cs

toGarden :: Grid -> Garden
toGarden grid = M.foldlWithKey' (\garden (x, y) crop -> M.insert (x, y) (crop, getFences grid (x, y), getCorners grid (x, y)) garden) M.empty grid

getCorners :: Grid -> Coord -> Int
getCorners grid (x, y) = length $ filter id (internalCorners ++ externalCorners)
  where
    crop = grid M.! (x, y)
    n = M.lookup (x - 1, y) grid == Just crop
    e = M.lookup (x, y + 1) grid == Just crop
    s = M.lookup (x + 1, y) grid == Just crop
    w = M.lookup (x, y - 1) grid == Just crop
    ne = M.lookup (x - 1, y + 1) grid == Just crop
    se = M.lookup (x + 1, y + 1) grid == Just crop
    sw = M.lookup (x + 1, y - 1) grid == Just crop
    nw = M.lookup (x - 1, y - 1) grid == Just crop
    internalCorners = [not n && not e, not s && not e, not s && not w, not n && not w]
    externalCorners = [n && e && not ne, s && e && not se, s && w && not sw, n && w && not nw]

getFences :: Grid -> Coord -> Int
getFences grid (x, y) = length $ filter id $ map (\v -> M.lookup v grid /= Just crop) [(x - 1, y), (x, y + 1), (x + 1, y), (x, y - 1)]
  where
    crop = grid M.! (x, y)

getRegions :: Grid -> CropRegions
getRegions = M.foldlWithKey' addCropToRegion M.empty

addCropToRegion :: CropRegions -> Coord -> Crop -> CropRegions
addCropToRegion cropRegions (x, y) crop = M.insertWith (\_ oldRegions -> addToRegions oldRegions crop (x, y)) crop [S.fromList [(x, y)]] cropRegions

addToRegions :: Regions -> Crop -> Coord -> Regions
addToRegions regions crop coord = newRegions
  where
    (mergedRegion, otherRegions) = foldl' (\(mr, or) r -> if any (isAdjacent coord) r then (S.union r mr, or) else (mr, r : or)) (S.empty, []) regions
    newRegions = S.insert coord mergedRegion : otherRegions

getPrice :: Garden -> CropRegions -> Int
getPrice garden cropRegions = M.foldl' (\acc regionCost -> sum regionCost + acc) 0 regionCosts
  where
    regionCosts = M.foldlWithKey' (\acc crop regions -> M.insert crop (map (\r -> getPerimeter garden crop r * length r) regions) acc) M.empty cropRegions

getPriceBulkDiscount :: Garden -> CropRegions -> Int
getPriceBulkDiscount garden cropRegions = M.foldl' (\acc regionCost -> sum regionCost + acc) 0 regionCosts
  where
    regionCosts = M.foldlWithKey' (\acc crop regions -> M.insert crop (map (\r -> getNSides garden crop r * length r) regions) acc) M.empty cropRegions

getPerimeter :: Garden -> Crop -> Region -> Int
getPerimeter garden crop region = M.foldlWithKey' (\acc coord (c, nf, nc) -> if c == crop && S.member coord region then nf + acc else acc) 0 garden

getNSides :: Garden -> Crop -> Region -> Int
getNSides garden crop region = M.foldlWithKey' (\acc coord (c, nf, nc) -> if c == crop && S.member coord region then nc + acc else acc) 0 garden

isAdjacent :: Coord -> Coord -> Bool
isAdjacent (a, b) (c, d) = (a == c + 1 && b == d) || (a == c - 1 && b == d) || (a == c && b == d + 1) || (a == c && b == d - 1)