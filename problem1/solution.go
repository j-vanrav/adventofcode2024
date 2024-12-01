package main

import (
	"bufio"
	"fmt"
	"log"
	"os"
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
	fmt.Println(list1, list2)
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
