package main

import "fmt"

func double(n any) any {
	return (n * 2)
}

func init() {
	fmt.Println(double(3))
}
func init() {
	fmt.Println(double(6))
}
var base = 10
var offset = func() any {
	fmt.Println("Computing offset")
	return 25
}()

func main() {
	fmt.Println(base)
	fmt.Println(offset)
	fmt.Println((base + offset))
}
func init() {
	fmt.Println(double(9))
}
