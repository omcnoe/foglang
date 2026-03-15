package main

import "fmt"

func fib(n any) any {
	if (n <= 1) {
		return n
	} else {
		return (fib((n - 1)) + fib((n - 2)))
	}
}

func main() {
	fmt.Println(fib(10))
}
