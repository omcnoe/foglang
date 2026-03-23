package main

import "fmt"

var sliceCompact []int = []int{1, 2, 3}
var sliceSpaced []int = []int{4, 5, 6}
var sliceNested [][]int = [][]int{[]int{1, 2}, []int{3, 4}}
var sliceNestedSpaced [][]int = [][]int{[]int{7, 8}, []int{9, 10}}
var mapCompact map[string]int = map[string]int{}
var mapSpaced map[string]int = map[string]int{}
var mapWideSpaced map[string]int = map[string]int{}
var mapSliceVal map[int][]int = map[int][]int{}
var mapSliceValSpaced map[int][]int = map[int][]int{}
var sliceFolded []int = []int{100}
var mapFolded map[int][]int = map[int][]int{}
var mapKeyFolded map[int][]string = map[int][]string{}

func f1(x int) int {
	return (x * 2)
}
func f2(a int, b int) int {
	return (a * b)
}
func applyF(f func(int) int, x int) int {
	return f(x)
}
func applyF2(f func(int, int) int, x int, y int) int {
	return f(x, y)
}
func applyBlockLevel(f func(int, int) int, x int, y int) int {
	return f(x, y)
}
func applyLineFolded() int {
	return func(g func(int, int) int, x int, y int) int { return g(x, y) }(f2, 3, 4)
}
func main() {
	fmt.Println(sliceCompact)
	fmt.Println(sliceSpaced)
	fmt.Println(sliceNested)
	fmt.Println(sliceNestedSpaced)
	fmt.Println(mapCompact)
	fmt.Println(mapSpaced)
	fmt.Println(mapWideSpaced)
	fmt.Println(mapSliceVal)
	fmt.Println(mapSliceValSpaced)
	fmt.Println(sliceFolded)
	fmt.Println(mapFolded)
	fmt.Println(mapKeyFolded)
	fmt.Println(applyF(f1, 21))
	fmt.Println(applyF2(f2, 10, 20))
	fmt.Println(applyBlockLevel(f2, 3, 4))
	fmt.Println(applyLineFolded())
}
