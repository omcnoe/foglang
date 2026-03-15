package main

import "fmt"

func main() {
	myDoubler := func(x any) any { return (2 * x) }
	fmt.Println(myDoubler(2))
}
