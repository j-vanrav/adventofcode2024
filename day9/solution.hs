{-# LANGUAGE MultiWayIf #-}

module Main where

import Data.Function ((&))
import Data.Maybe (fromMaybe)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable (MVector)
import Data.Vector.Mutable qualified as MV
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
compressFile file = reverse $ go (V.fromList (reverse file)) []
  where
    go file acc =
      if
        | V.length file == 0 -> acc
        | V.head file == '.' -> go (V.tail file) acc
        | V.last file == '.' -> go (V.init $ V.tail file) (V.head file : acc)
        | otherwise -> go (V.init file) (V.last file : acc)

calculateChecksum :: File -> Int
calculateChecksum file = sum [fromMaybe 0 (readMaybe [file !! i]) * i | i <- [0 .. length file - 1]]

part1 :: String -> Int
part1 content = content & parseDiskMap & parseFile & compressFile & calculateChecksum

traceVal :: (Show a) => String -> a -> a
traceVal s a = trace (s ++ show a) a