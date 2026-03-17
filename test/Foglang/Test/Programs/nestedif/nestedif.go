package main

import "fmt"

func classify(n int) int {
	if (n < 10) {
		return 0
	} else if (n < 100) {
		if (n < 50) {
			if (n < 25) {
				return 1
			} else {
				return 2
			}
		} else {
			return 3
		}
	} else {
		return 4
	}
}

func classifyBusted(n int) int {
	if (n < 10) {
		return 0
	} else if (n < 100) {
		if (n < 50) {
			if (n < 25) {
				return 1
			} else {
				return 2
			}
		} else {
			return 3
		}
	} else {
		return 4
	}
}

func main() {
	fmt.Println(classify(5))
	fmt.Println(classify(15))
	fmt.Println(classify(35))
	fmt.Println(classify(75))
	fmt.Println(classify(200))
	fmt.Println(classifyBusted(5))
	fmt.Println(classifyBusted(15))
	fmt.Println(classifyBusted(35))
	fmt.Println(classifyBusted(75))
	fmt.Println(classifyBusted(200))
}
