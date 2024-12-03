package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("../input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	numbers, err := parseFile(file)
	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(calcSafeLines(numbers))
}

func calcSafeLines(numbers [][]int) (safeCount int) {
	for _, v := range numbers {
		if isIncreasingSafely(v) {
			safeCount++
		}
	}
	return
}

func parseFile(file *os.File) (numbers [][]int, err error) {
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		if line == "" {
			continue
		}
		parts := strings.Split(line, " ")
		ns := []int{}
		for _, p := range parts {
			var result int
			result, err = strconv.Atoi(strings.TrimSpace(p))
			if err != nil {
				return
			}
			ns = append(ns, result)
		}
		numbers = append(numbers, ns)
	}
	return
}

func isIncreasingSafely(row []int) bool {
	isIncreasing := false
	isDecreasing := false
	for i, v := range row {
		if i == 0 {
			continue
		}
		if !isSafeInterval(row[i-1], v) {
			return false
		}
		if row[i-1] < v {
			isIncreasing = true
		}
		if row[i-1] > v {
			isDecreasing = true
		}
		if isIncreasing && isDecreasing {
			return false
		}
	}
	return true
}

func isSafeInterval(n1 int, n2 int) bool {
	return n1 != n2 && (absInt(n2-n1) <= 3)
}

func absInt(v int) int {
	return int(math.Abs(float64(v)))
}
