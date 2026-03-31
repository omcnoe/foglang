package main

import "fmt"

var standard int = 1
var wideIndent int = 42

func multiItem() {
	fmt.Println("a")
	fmt.Println("b")
	fmt.Println("c")
}

var nested int = func() int {
	a := func() int {
		b := func() int {
			c := 1
			_ = c
			return (c + 1)
		}()
		_ = b
		return (b + 1)
	}()
	_ = a
	return (a + 1)
}()
var folded int = ((1 + 2) + 3)

func foldedThenStmt() {
	_ = (1 + 2)
	fmt.Println("after the expression")
}

var deepFold int = (((1 + 2) + 3) + 4)

func orChain(a bool, b bool, c bool) bool {
	return ((a || b) || c)
}
func andChain(a bool, b bool, c bool, d bool) bool {
	return (((a && b) && c) && d)
}
func bitwiseOrFlat(a int, b int) int {
	return (a | b)
}
func mulChain(x int) int {
	return ((x * 2) * 3)
}

var minusCont int = (10 - 3)

func compared(x int) bool {
	return (x > 0)
}
func matchStd(x int) string {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		return "zero"
	} else if _scrut1 == 1 {
		return "one"
	} else {
		return "other"
	}
}
func matchMessy(x int) string {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		return "zero"
	} else if _scrut1 == 1 {
		return "one"
	} else {
		return "other"
	}
}
func matchNested(x int, y int) string {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		_scrut2 := y
		_ = _scrut2
		if _scrut2 == 0 {
			return "both zero"
		} else {
			return "x zero"
		}
	} else {
		return "x nonzero"
	}
}
func matchFlush(x int) string {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		return "zero"
	} else {
		return "other"
	}
}
func matchBitwiseOr(x int, y int) int {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 1 {
		return (y | 3)
	} else if _scrut1 == 2 {
		return y
	} else {
		return 0
	}
}
func matchBitwiseOrFlat(x int, y int) int {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 1 {
		return (y | 3)
	} else if _scrut1 == 2 {
		return y
	} else {
		return 0
	}
}
func matchPipeAtLineIndent(x int) int {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 1 {
		return 10
	} else if _scrut1 == 2 {
		return 20
	} else {
		return 0
	}
}
func matchPipeNoLHS(x int) string {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		return "zero"
	} else if _scrut1 == 1 {
		return "one"
	} else {
		return "other"
	}
}
func seqBody() int {
	fmt.Println("computing")
	return 42
}
func seqSemicolon() int {
	fmt.Println("a")
	fmt.Println("b")
	return 42
}
func seqSemicolonLet() int {
	x := 10
	_ = x
	return (x + 1)
}
func seqMixed() int {
	fmt.Println("a")
	fmt.Println("b")
	fmt.Println("c")
	fmt.Println("d")
	return 42
}
func seqSemiBranch(x int) int {
	if x > 0 {
		fmt.Println("positive")
		return x
	} else {
		fmt.Println("non-positive")
		return (0 - x)
	}
}
func seqSemiMatch(x int) string {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		fmt.Println("zero")
		return "zero"
	} else {
		fmt.Println("other")
		return "other"
	}
}
func seqSemiNested() int {
	x := 1
	_ = x
	y := 2
	_ = y
	return (x + y)
}
func seqBranch(x int) int {
	if x > 0 {
		fmt.Println("positive")
		return x
	} else {
		fmt.Println("non-positive")
		return (0 - x)
	}
}
func seqCond(x int) int {
	if func() bool {
		positive := (x > 0)
		_ = positive
		return positive
	}() {
		return x
	} else {
		return (0 - x)
	}
}
func elseIfChain(x int) string {
	if x == 1 {
		return "one"
	} else if x == 2 {
		return "two"
	} else if x == 3 {
		return "three"
	} else {
		return "other"
	}
}
func elseIfSeq(x int) int {
	if x > 0 {
		fmt.Println("positive")
		return x
	} else if x == 0 {
		fmt.Println("zero")
		return 0
	} else {
		fmt.Println("negative")
		return (0 - x)
	}
}
func elseIfSeqCond(x int) string {
	if func() bool {
		big := (x > 100)
		_ = big
		return big
	}() {
		return "big"
	} else if func() bool {
		medium := (x > 10)
		_ = medium
		return medium
	}() {
		return "medium"
	} else {
		return "small"
	}
}

var seqParen int = func() int {
	x := 7
	_ = x
	return (x + 1)
}()

func seqArm(x int) int {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		fmt.Println("got zero")
		return 0
	} else {
		n := _scrut1
		_ = n
		fmt.Println("got nonzero")
		return (n * 2)
	}
}

var parenReset int = func() int {
	f := func(x int) int { return (x * 2) }
	_ = f
	return f(21)
}()

func parenFlat(a bool, b bool, c bool) bool {
	return ((a || b) || c)
}

var parenNest int = (1 + (2 + 3))

func combo(xs []int) int {
	_scrut1 := xs
	_ = _scrut1
	if len(_scrut1) == 0 {
		return 0
	} else if len(_scrut1) > 0 {
		x := _scrut1[0]
		_ = x
		rest := _scrut1[1:]
		_ = rest
		doubled := (x * 2)
		_ = doubled
		return doubled
	}
	panic("match not exhaustive")
}

var funcMultiLine func(int) int = func(x int) int {
	fmt.Println("computing")
	return (x * 2)
}

func applyToTen(f func(int) int) int {
	return f(10)
}
func comboLambda() {
	fmt.Println(applyToTen(func(x int) int {
		return func() int {
			y := (x + 1)
			_ = y
			return (y * 2)
		}()
	}))
}
func main() {
	fmt.Println(standard)
	fmt.Println(wideIndent)
	multiItem()
	fmt.Println(nested)
	fmt.Println(folded)
	foldedThenStmt()
	fmt.Println(deepFold)
	fmt.Println(orChain(true, false, true))
	fmt.Println(andChain(true, true, true, true))
	fmt.Println(bitwiseOrFlat(5, 3))
	fmt.Println(mulChain(5))
	fmt.Println(minusCont)
	fmt.Println(compared(5))
	fmt.Println(matchStd(0))
	fmt.Println(matchStd(1))
	fmt.Println(matchStd(42))
	fmt.Println(matchMessy(0))
	fmt.Println(matchMessy(1))
	fmt.Println(matchMessy(42))
	fmt.Println(matchNested(0, 0))
	fmt.Println(matchNested(0, 1))
	fmt.Println(matchFlush(0))
	fmt.Println(matchBitwiseOr(1, 4))
	fmt.Println(matchBitwiseOr(2, 4))
	fmt.Println(matchBitwiseOrFlat(1, 4))
	fmt.Println(matchBitwiseOrFlat(2, 4))
	fmt.Println(matchPipeAtLineIndent(1))
	fmt.Println(matchPipeAtLineIndent(2))
	fmt.Println(matchPipeNoLHS(0))
	fmt.Println(matchPipeNoLHS(1))
	fmt.Println(matchPipeNoLHS(42))
	fmt.Println(seqBody())
	fmt.Println(seqSemicolon())
	fmt.Println(seqSemicolonLet())
	fmt.Println(seqMixed())
	fmt.Println(seqSemiBranch(5))
	fmt.Println(seqSemiBranch((0 - 3)))
	fmt.Println(seqSemiMatch(0))
	fmt.Println(seqSemiMatch(42))
	fmt.Println(seqSemiNested())
	fmt.Println(seqBranch(5))
	fmt.Println(seqBranch((0 - 3)))
	fmt.Println(elseIfChain(1))
	fmt.Println(elseIfChain(2))
	fmt.Println(elseIfChain(3))
	fmt.Println(elseIfChain(99))
	fmt.Println(elseIfSeq(5))
	fmt.Println(elseIfSeq(0))
	fmt.Println(elseIfSeq((0 - 3)))
	fmt.Println(elseIfSeqCond(200))
	fmt.Println(elseIfSeqCond(50))
	fmt.Println(elseIfSeqCond(5))
	fmt.Println(seqCond(5))
	fmt.Println(seqCond((0 - 3)))
	fmt.Println(seqParen)
	fmt.Println(seqArm(0))
	fmt.Println(seqArm(5))
	fmt.Println(parenReset)
	fmt.Println(parenFlat(true, false, true))
	fmt.Println(parenNest)
	fmt.Println(combo([]int{3}))
	fmt.Println(combo([]int{}))
	fmt.Println(funcMultiLine(5))
	comboLambda()
}
