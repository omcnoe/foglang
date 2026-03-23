package main

import "fmt"

var greeting string = "hello"
var pi float64 = 3.14
var flag bool = true

func add(x int, y int) int {
	return ((x + y) + 1)
}
func double(x int) int {
	return (x * 2)
}
func fib(n int) int {
	if n < 2 {
		return n
	} else {
		return (fib((n - 1)) + fib((n - 2)))
	}
}
func is_positive(x int) bool {
	return (x > 0)
}
func abs_val(x int) int {
	if x < 0 {
		return (0 - x)
	} else {
		return x
	}
}

var nums []int = []int{1, 2, 3}

func first(xs []int) int {
	return xs[0]
}
func both(x bool, y bool) bool {
	return (x && y)
}
func apply_fn(f func(int) int, x int) int {
	return f(x)
}
func apply_lambda(x int) int {
	return apply_fn(func(n int) int { return (n * 3) }, x)
}
func main() {
	fmt.Println(greeting)
	fmt.Println(pi)
	fmt.Println(flag)
	fmt.Println(add(2, 3))
	fmt.Println(double(5))
	fmt.Println(fib(10))
	fmt.Println(is_positive(5))
	fmt.Println(abs_val((0 - 3)))
	fmt.Println(nums)
	fmt.Println(first(nums))
	fmt.Println(both(true, false))
	fmt.Println(apply_fn(double, 7))
	fmt.Println(apply_lambda(4))
}
