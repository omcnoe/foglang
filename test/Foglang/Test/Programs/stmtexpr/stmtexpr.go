package main

import "fmt"

func abs(x int) int {
	if x < 0 {
		return (0 - x)
	} else {
		return x
	}
}
func printSign(x int) {
	if x < 0 {
		fmt.Println("negative")
	} else if x == 0 {
		fmt.Println("zero")
	} else {
		fmt.Println("positive")
	}
}
func letExpr(x int) int {
	a := (x + 1)
	_ = a
	b := (a * 2)
	_ = b
	return (a + b)
}
func letUnitThenValue(x int) int {
	fmt.Println("side effect")
	return (x * 10)
}
func condAdd(x int, y int) int {
	return (x + func() int {
		if y > 0 {
			return y
		} else {
			return (0 - y)
		}
	}())
}
func opsAsStmts() {
	_ = (1 + 2)
	_ = (3 * 4)
	fmt.Println("done")
}
func stmtsThenExpr(x int) int {
	fmt.Println("first")
	fmt.Println("second")
	fmt.Println("third")
	return x
}
func localFuncUse() {
	var double func(n int) int
	double = func(n int) int {
		return (n * 2)
	}
	_ = double
	fmt.Println(double(21))
}
func applyToTen(f func(int) int) int {
	return f(10)
}
func lambdaArg() {
	fmt.Println(applyToTen(func(x int) int { return (x + 5) }))
}
func clamp(lo int, hi int, x int) int {
	if x < lo {
		return lo
	} else if x > hi {
		return hi
	} else {
		return x
	}
}
func init() {
	fmt.Println("init side effect")
}

var simpleVar int = 7
var exprVar int = (3 + 4)
var condVar int = func() int {
	if true {
		return 1
	} else {
		return 0
	}
}()
var letVar int = func() int {
	x := 10
	_ = x
	return (x + 5)
}()
var computed int = func() int {
	fmt.Println("computing global")
	return 42
}()

func ifInLet(x int) int {
	sign := func() int {
		if x < 0 {
			return (0 - 1)
		} else {
			return 1
		}
	}()
	_ = sign
	return (x * sign)
}
func lambdaInLet() {
	f := func(x int) int { return (x * x) }
	_ = f
	fmt.Println(f(7))
}
func constVal() int {
	return 42
}
func stmtsThenIf(x int) int {
	fmt.Println("checking")
	if x > 0 {
		return x
	} else {
		return (0 - x)
	}
}
func main() {
	fmt.Println(abs(5))
	fmt.Println(abs((0 - 3)))
	printSign(5)
	printSign(0)
	printSign((0 - 1))
	fmt.Println(letExpr(3))
	fmt.Println(letUnitThenValue(4))
	fmt.Println(condAdd(10, 5))
	fmt.Println(condAdd(10, (0 - 5)))
	opsAsStmts()
	fmt.Println(stmtsThenExpr(99))
	localFuncUse()
	lambdaArg()
	fmt.Println(clamp(0, 100, 50))
	fmt.Println(clamp(0, 100, (0 - 5)))
	fmt.Println(clamp(0, 100, 200))
	fmt.Println(simpleVar)
	fmt.Println(exprVar)
	fmt.Println(condVar)
	fmt.Println(letVar)
	fmt.Println(computed)
	fmt.Println(ifInLet(5))
	fmt.Println(ifInLet((0 - 3)))
	lambdaInLet()
	fmt.Println(constVal())
	fmt.Println(stmtsThenIf(5))
	fmt.Println(stmtsThenIf((0 - 3)))
}
