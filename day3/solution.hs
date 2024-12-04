module Main where

import Text.Regex.TDFA (getAllTextMatches, (=~))

main = do
  content <- readFile "./day3/input"
  let pattern = "mul\\((\\d+),(\\d+)\\)"
  print (sum (map (mult . getNums) (getMuls content)))

getMuls :: String -> [String]
getMuls c = do
  let x = (c :: String) =~ "mul\\(([0-9]+),([0-9]+)\\)" :: (String, String, String)
      hasMatch = not (null (_snd x))
  if hasMatch then _snd x : getMuls (thrd x) else []

getNums :: String -> [Int]
getNums c = map read (getAllTextMatches ((c :: String) =~ "[0-9]+") :: [String])

_snd :: (a, b, c) -> b
_snd (_, y, _) = y

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

mult :: [Int] -> Int
mult [x, y] = x * y