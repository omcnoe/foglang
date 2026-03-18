package main

import "fmt"

func main() {
	myDoublerLocal := func(x int) int {
		return (2 * x)
	}
	myDoublerLambda := func(x int) int { return (2 * x) }
	multiLineLambda := func(x int) int {
		fmt.Println("doubling")
		return (x * 2)
	}
	fmt.Println(myDoublerLocal(2))
	fmt.Println(myDoublerLambda(2))
	fmt.Println(func(x int) int { return (2 * x) }(2))
	fmt.Println(multiLineLambda(5))
}
