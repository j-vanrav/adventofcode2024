package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"os"
	"sort"
	"strconv"
	"strings"
)

func main() {
	file, err := os.Open("./input")
	if err != nil {
		log.Fatal(err)
	}
	defer file.Close()
	list1, list2, err := parseFile(file)
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println(calcDistance(list1, list2))
	fmt.Println(calcSimilarity(list1, list2))
}

func parseFile(file *os.File) (numbers1 []int, numbers2 []int, err error) {
	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := scanner.Text()
		parts := strings.Split(line, "   ")
		left, err1 := strconv.Atoi(strings.TrimSpace(parts[0]))
		right, err2 := strconv.Atoi(strings.TrimSpace(parts[1]))
		if err1 != nil {
			err = err1
			return
		}
		if err2 != nil {
			err = err2
			return
		}
		numbers1 = append(numbers1, left)
		numbers2 = append(numbers2, right)
	}
	return
}

func calcDistance(list1 []int, list2 []int) (sum int) {
	sort.Slice(list1, func(i, j int) bool {
		return list1[i] < list1[j]
	})
	sort.Slice(list2, func(i, j int) bool {
		return list2[i] < list2[j]
	})
	for i := range list1 {
		sum += int(math.Abs((float64(list2[i]) - float64(list1[i]))))
	}
	return
}

func calcSimilarity(list1 []int, list2 []int) (sum int) {
	sort.Slice(list1, func(i, j int) bool {
		return list1[i] < list1[j]
	})
	sort.Slice(list2, func(i, j int) bool {
		return list2[i] < list2[j]
	})
	for _, i := range list1 {
		var numSimilar int
		for _, j := range list2 {
			if i > j {
				continue
			}
			if i == j {
				numSimilar++
				continue
			}
			if i < j {
				break
			}
		}
		sum += (i * numSimilar)
	}
	return
}
