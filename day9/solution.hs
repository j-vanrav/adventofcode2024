{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
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
compressFile file = reverse $ go file []
  where
    go [] acc = acc
    go file acc =
      if
        | last file == '.' -> go (init file) acc
        | head file == '.' -> go (init $ tail file) (last file : acc)
        | otherwise -> go (tail file) (head file : acc)

calculateChecksum :: File -> Int
calculateChecksum file = sum [fromMaybe 0 (readMaybe [file !! i]) * i | i <- [0 .. length file - 1]]

part1 :: String -> Int
part1 content = content & parseDiskMap & parseFile & compressFile & calculateChecksum
