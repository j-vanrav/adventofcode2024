My solutions for [advent of code](https://adventofcode.com/) 2024

#### Quick start:

##### Go:

1. Install Go
2. Run like `go run ./day1/solution.go`

##### Haskell

1. Install Haskell & Cabal
2. Run like `cabal run day1` or `ghci ./day1/solution.hs` + `main`

| Time O() :: Space O() complexity | Go         | Haskell      | Theorised optimum |
| -------------------------------- | ---------- | ------------ | ----------------- |
| Day 1: Part 1 (distance)         | NlogN :: N | N :: 1       | N :: 1            |
| Day 1: Part 2 (similarity)       | N^2 :: N   | N^2 :: 1     | NlogN :: 1        |
| Day 2: Part 1 (safety)           | NM :: NM   | NM :: 1      | NM :: 1           |
| Day 2: Part 2 (dampening)        | NM :: NM   | NM^3 :: NM^2 | NM :: 1           |
| Day 3: Part 1 (corrupted)        | NM :: N    |              |                   |
| Day 3: Part 2 (disabling)        |            | NM :: N      |                   |
