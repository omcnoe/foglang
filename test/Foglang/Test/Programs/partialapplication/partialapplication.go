package main

import "fmt"

func add(a int, b int) int {
	return (a + b)
}

func main() {
	add5 := func(_p0 int) int { return add(5, _p0) }
	fmt.Println(add5(3))
	fmt.Println(add5(10))
	fmt.Println(add(2, 3))
}
