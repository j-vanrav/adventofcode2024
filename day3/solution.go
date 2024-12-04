package main

import (
	"fmt"
	"log"
	"os"
	"regexp"
	"strconv"
)

func main() {
	content, err := os.ReadFile("./day3/input")
	if err != nil {
		log.Fatal(err)
	}
	re := regexp.MustCompile(`mul\((\d*),(\d*)\)`)
	matches := re.FindAllStringSubmatch(string(content), -1)
	count := 0
	for _, m := range matches {
		int1, err := strconv.Atoi(m[len(m)-2])
		if err != nil {
			log.Fatal(err)
		}
		int2, err := strconv.Atoi(m[len(m)-1])
		if err != nil {
			log.Fatal(err)
		}
		count += (int1 * int2)
	}
	fmt.Println(count)
}
