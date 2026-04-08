package main

import "fmt"

func describe(x int) string {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		return "zero"
	} else if _scrut1 == 1 {
		return "one"
	} else {
		return "other"
	}
	panic("runtime error: non-exhaustive match")
}
func testSiblings(x int) int {
	var double func(n int) int
	double = func(n int) int {
		_scrut2 := n
		_ = _scrut2
		if _scrut2 == 0 {
			return 0
		} else {
			return (n * 2)
		}
		panic("runtime error: non-exhaustive match")
	}
	_ = double
	var triple func(n int) int
	triple = func(n int) int {
		_scrut2 := n
		_ = _scrut2
		if _scrut2 == 0 {
			return 0
		} else {
			return (n * 3)
		}
		panic("runtime error: non-exhaustive match")
	}
	_ = triple
	var quadruple func(n int) int
	quadruple = func(n int) int {
		_scrut2 := n
		_ = _scrut2
		if _scrut2 == 0 {
			return 0
		} else {
			return (n * 4)
		}
		panic("runtime error: non-exhaustive match")
	}
	_ = quadruple
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		fmt.Println("x is zero")
	} else if _scrut1 == 1 {
		fmt.Println("x is one")
	} else {
		fmt.Println("x is nonzero")
	}
	result := ((double(x) + triple(x)) + quadruple(x))
	_ = result
	return result
}
func first(x int) int {
	var a func(n int) int
	a = func(n int) int {
		_scrut2 := n
		_ = _scrut2
		if _scrut2 == 0 {
			return 0
		} else {
			return (n * 2)
		}
		panic("runtime error: non-exhaustive match")
	}
	_ = a
	var b func(n int) int
	b = func(n int) int {
		_scrut2 := n
		_ = _scrut2
		if _scrut2 == 0 {
			return 0
		} else {
			return (n * 3)
		}
		panic("runtime error: non-exhaustive match")
	}
	_ = b
	var inner func(candidates []int) int
	inner = func(candidates []int) int {
		if 1 == 0 {
			return 0
		} else {
			_scrut3 := candidates
			_ = _scrut3
			if len(_scrut3) == 0 {
				return 0
			} else if len(_scrut3) > 0 {
				x := _scrut3[0]
				_ = x
				rest := _scrut3[1:]
				_ = rest
				_scrut4 := x
				_ = _scrut4
				if _scrut4 == 0 {
					return a(x)
				} else {
					r := b(x)
					_ = r
					return r
				}
				panic("runtime error: non-exhaustive match")
			}
			panic("runtime error: non-exhaustive match")
		}
	}
	_ = inner
	return inner([]int{x})
}
func second(x int) int {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		return 0
	} else {
		return (x * 3)
	}
	panic("runtime error: non-exhaustive match")
}
func greet(name string) string {
	_scrut1 := name
	_ = _scrut1
	if _scrut1 == "Alice" {
		return "hello Alice!"
	} else if _scrut1 == "Bob" {
		return "hey Bob"
	} else {
		return "hi stranger"
	}
	panic("runtime error: non-exhaustive match")
}
func identity(x int) int {
	_scrut1 := x
	_ = _scrut1
	if true {
		return x
	}
	panic("runtime error: non-exhaustive match")
}
func identity2(x int) int {
	_scrut1 := x
	_ = _scrut1
	if true {
		y := _scrut1
		_ = y
		return (y + 1)
	}
	panic("runtime error: non-exhaustive match")
}
func printIt(x int) {
	_scrut1 := x
	_ = _scrut1
	if true {
		fmt.Println(x)
	}
}
func main() {
	fmt.Println(describe(0))
	fmt.Println(describe(1))
	fmt.Println(describe(42))
	fmt.Println(testSiblings(5))
	fmt.Println(first(3))
	fmt.Println(second(4))
	fmt.Println(greet("Alice"))
	fmt.Println(greet("Bob"))
	fmt.Println(greet("Charlie"))
	fmt.Println(identity(42))
	fmt.Println(identity2(10))
	printIt(99)
}
