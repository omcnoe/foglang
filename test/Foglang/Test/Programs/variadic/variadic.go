package main

import "fmt"

func printf(format string, args ...any) {
	fmt.Printf(format, args...)
}
func main() {
	printf("hello, world!\n")
	printf("%d + %d = %d\n", 1, 2, 3)
	printInt := func(_args ...any) { printf("%d\n", _args...) }
	printInt(42)
	printInt()
}
