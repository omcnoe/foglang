package main

import "fmt"

func main() {
	var myDoublerLocal func(x int) int
	myDoublerLocal = func(x int) int {
		return (2 * x)
	}
	_ = myDoublerLocal
	myDoublerLambda := func(x int) int { return (2 * x) }
	_ = myDoublerLambda
	multiLineLambda := func(x int) int {
		fmt.Println("doubling")
		return (x * 2)
	}
	_ = multiLineLambda
	fmt.Println(myDoublerLocal(2))
	fmt.Println(myDoublerLambda(2))
	fmt.Println(func(x int) int { return (2 * x) }(2))
	fmt.Println(multiLineLambda(5))
}
