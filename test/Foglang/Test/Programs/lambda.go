package main

import "fmt"

func main() {
	myDoubler := func(x int) int { return (2 * x) }
	fmt.Println(myDoubler(2))
}
