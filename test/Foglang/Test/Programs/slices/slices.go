package main

import "fmt"

func printLen(xs []int) {
	fmt.Println(len(xs))
}
func getIndex(xs []int, i int) {
	fmt.Println(xs[i])
}
func main() {
	xs := []int{10, 20, 30}
	_ = xs
	printLen(xs)
	getIndex(xs, 0)
	getIndex(xs, 2)
	fmt.Println(append([]int{0}, xs...))
	fmt.Println(nil)
}
