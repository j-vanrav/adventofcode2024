package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
	"strings"
)

func main() {
	file, err := os.Open("./day4/input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	table, err := parseFile(file)
	if err != nil {
		log.Fatal(err)
	}
	XMASCount := 0
	CrossMAXCount := 0
	for i := range table {
		for j := range table[i] {
			XMASCount += XMASMatchesFrom(table, i, j)
			CrossMAXCount += CrossMASMatchesFrom(table, i, j)
		}
	}

	fmt.Println(XMASCount, CrossMAXCount)
}

func parseFile(file *os.File) (table [][]string, err error) {
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		chars := strings.Split(line, "")
		table = append(table, chars)
	}
	return
}

func XMASMatchesFrom(table [][]string, x int, y int) (matches int) {
	if len(table) == 0 {
		return
	}
	up, down, right, left := y-3 >= 0, y+3 < len(table), x+3 < len(table), x-3 >= 0
	if up {
		if (table[x][y] + table[x][y-1] + table[x][y-2] + table[x][y-3]) == "XMAS" {
			matches++
		}
	}
	if up && right {
		if (table[x][y] + table[x+1][y-1] + table[x+2][y-2] + table[x+3][y-3]) == "XMAS" {
			matches++
		}
	}
	if right {
		if (table[x][y] + table[x+1][y] + table[x+2][y] + table[x+3][y]) == "XMAS" {
			matches++
		}
	}
	if down && right {
		if (table[x][y] + table[x+1][y+1] + table[x+2][y+2] + table[x+3][y+3]) == "XMAS" {
			matches++
		}
	}
	if down {
		if (table[x][y] + table[x][y+1] + table[x][y+2] + table[x][y+3]) == "XMAS" {
			matches++
		}
	}
	if down && left {
		if (table[x][y] + table[x-1][y+1] + table[x-2][y+2] + table[x-3][y+3]) == "XMAS" {
			matches++
		}
	}
	if left {
		if (table[x][y] + table[x-1][y] + table[x-2][y] + table[x-3][y]) == "XMAS" {
			matches++
		}
	}
	if up && left {
		if (table[x][y] + table[x-1][y-1] + table[x-2][y-2] + table[x-3][y-3]) == "XMAS" {
			matches++
		}
	}
	return
}

func CrossMASMatchesFrom(table [][]string, x int, y int) (matches int) {
	if len(table) == 0 {
		return
	}
	if !(y-1 >= 0 && y+1 < len(table) && x+1 < len(table) && x-1 >= 0) {
		return
	}
	C := table[x][y]

	if C != "A" {
		return
	}

	NE := table[x+1][y-1]
	SE := table[x+1][y+1]
	SW := table[x-1][y+1]
	NW := table[x-1][y-1]

	up := NW == "M" && NE == "M" && SW == "S" && SE == "S"
	down := NW == "S" && NE == "S" && SW == "M" && SE == "M"
	right := NW == "S" && NE == "M" && SW == "S" && SE == "M"
	left := NW == "M" && NE == "S" && SW == "M" && SE == "S"

	if up || down || left || right {
		matches++
	}

	return
}
