package main

import "fmt"

func mult(a float64, b float64) float64 {
	return (a * b)
}

func init() {
	fmt.Println(mult(2.0, 3.0))
}
func init() {
	fmt.Println(mult(0.5, 6.0))
}
var base int = 10
var offset int = func() int {
	fmt.Println("Computing offset")
	return 25
}()

func main() {
	fmt.Println(base)
	fmt.Println(offset)
	fmt.Println((base + offset))
}
func init() {
	fmt.Println(mult(1.5, 9.0))
}
