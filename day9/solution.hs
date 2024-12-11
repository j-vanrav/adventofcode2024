{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Text.Read (readMaybe)

main = do
  content <- readFile "./day9/input"
  print (part1 content)

type File = String

type DiskMap = [Int]

parseDiskMap :: String -> DiskMap
parseDiskMap = map (\c -> read [c])

parseFile :: DiskMap -> File
parseFile diskMap = concat [if even i then replicate (diskMap !! i) (head (show (i `div` 2))) else replicate (diskMap !! i) '.' | i <- [0 .. length diskMap - 1]]

compressFile :: File -> File
compressFile [] = []
compressFile [a] = [a]
compressFile file =
  if
    | last file == '.' -> compressFile (init file) ++ ['.']
    | head file == '.' -> [last file] ++ compressFile (init $ tail file) ++ [head file]
    | otherwise -> head file : compressFile (tail file)

calculateChecksum :: File -> Int
calculateChecksum file = sum [fromMaybe 0 (readMaybe [file !! i]) * i | i <- [0 .. length file - 1]]

part1 :: String -> Int
part1 content = content & parseDiskMap & parseFile & compressFile & calculateChecksum

traceVal :: (Show a) => a -> a
traceVal a = trace ("Trace " ++ show a) a