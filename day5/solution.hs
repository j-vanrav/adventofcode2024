module Main where

import Data.Bifunctor (bimap)
import Data.List.Split (splitOn)

main = do
  content <- readFile "./day5/input"
  let (rs, ps) = rulesAndPages . words $ content
      (rsi, psi) = (map rules rs, map pages ps)

  print (orderedSum rsi psi)

orderedSum :: [(Int, Int)] -> [[Int]] -> Int
orderedSum rsi psi = sum (map (\p -> if isOrdered p rsi then last (fst (splitHalf p)) else 0) psi)

rulesAndPages :: [String] -> ([String], [String])
rulesAndPages [x] = if '|' `elem` x then ([x], []) else ([], [x])
rulesAndPages (x : xs) = do
  let (rs, ps) = rulesAndPages [x]
  bimap (rs ++) (ps ++) (rulesAndPages xs)

rules :: String -> (Int, Int)
rules x = do
  let chars = splitOn "|" x
  (read (head chars), read (last chars))

pages :: String -> [Int]
pages x = map read (splitOn "," x)

isOrdered :: [Int] -> [(Int, Int)] -> Bool
isOrdered [x, y] rs = checkRules (x, y) rs
isOrdered (x : ys) rs = all (\y -> checkRules (x, y) rs) ys && isOrdered ys rs

checkRules :: (Int, Int) -> [(Int, Int)] -> Bool
checkRules (x, y) [(a, b)] = (x /= b) || (y /= a)
checkRules (x, y) ((a, b) : abs) = checkRules (x, y) [(a, b)] && checkRules (x, y) abs

splitHalf :: [a] -> ([a], [a])
splitHalf l = splitAt ((length l + 1) `div` 2) l