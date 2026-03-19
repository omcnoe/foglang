package main

import "fmt"

func applyN(f func(float64) float64, n int, x float64) float64 {
	if n <= 0 {
		return x
	} else {
		return applyN(f, (n - 1), f(x))
	}
}
func sqrt(x float64) float64 {
	improve := func(g float64) float64 { return ((g + (x / g)) / 2.0) }
	_ = improve
	return applyN(improve, 50, (x / 2.0))
}
func main() {
	fmt.Println(sqrt(2.0))
	fmt.Println(sqrt(9.0))
	fmt.Println(sqrt(25.0))
	fmt.Println(sqrt(144.0))
}
