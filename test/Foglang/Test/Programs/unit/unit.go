package main

import "fmt"

func zeroParam() {
	fmt.Println("zero param, zero return")
}
func structArg(x struct{}) {
	fmt.Println("struct arg, zero return")
}
func bindAndPass() {
	zeroParam()
	x := struct{}{}
	_ = x
	structArg(x)
}
func passDirect() {
	structArg(struct{}{})
}
func zeroParamStruct() struct{} {
	return struct{}{}
}
func bindStructResult() {
	s := zeroParamStruct()
	_ = s
	structArg(s)
}
func applyStruct(f func(struct{})) {
	f(struct{}{})
}
func withStructCallback() {
	applyStruct(func(x struct{}) { fmt.Println("callback") })
}
func withNamedStructCallback() {
	applyStruct(structArg)
}
func applyZero(f func()) {
	f()
}
func withZeroCallback() {
	applyZero(func() { fmt.Println("zero callback") })
}
func withNamedZeroCallback() {
	applyZero(zeroParam)
}

var unitReturnCoercedToStruct func() struct{} = func() struct{} { zeroParam(); return struct{}{} }
var structReturnCoercedToUnit func() = func() { zeroParamStruct() }

func printInt(n int) {
	fmt.Println(n)
}
func intToStruct(n int) struct{} {
	return struct{}{}
}

var unitReturnCoercedToStructI func(int) struct{} = func(_p0 int) struct{} { printInt(_p0); return struct{}{} }
var structReturnCoercedToUnitI func(int) = func(_p0 int) { intToStruct(_p0) }

func multiUnitParams(_p0 struct{}, _p1 struct{}, _p2 struct{}) {
	fmt.Println("multi unit params")
}
func applyMultiUnit(f func(struct{}, struct{}, struct{})) {
	f(struct{}{}, struct{}{}, struct{}{})
}

var discardUnit int = func() int {
	zeroParam()
	return 42
}()

func localCoerceToStruct() {
	f := func() struct{} { zeroParam(); return struct{}{} }
	_ = f
	f()
}
func localCoerceToUnit() {
	f := func() { zeroParamStruct() }
	_ = f
	f()
}
func localCoerceParamToStruct() {
	f := func(_p0 int) struct{} { printInt(_p0); return struct{}{} }
	_ = f
	f(1)
}
func localCoerceParamToUnit() {
	f := func(_p0 int) { intToStruct(_p0) }
	_ = f
	f(2)
}
func applyExpectStruct(f func() struct{}) {
	f()
}
func argCoerceToStruct() {
	applyExpectStruct(func() struct{} { zeroParam(); return struct{}{} })
}
func argCoerceToUnit() {
	applyZero(func() { zeroParamStruct() })
}
func getCoercedCallback() func() struct{} {
	return func() struct{} { zeroParam(); return struct{}{} }
}
func getCoercedCallbackUnit() func() {
	return func() { zeroParamStruct() }
}
func voidIfInExpr() int {
	if true {
		zeroParam()
	} else {
		zeroParam()
	}
	return 42
}
func voidMatchInExpr(x int) int {
	_scrut1 := x
	_ = _scrut1
	if _scrut1 == 0 {
		zeroParam()
	} else {
		zeroParam()
	}
	return 42
}
func voidSeqInExpr() int {
	zeroParam()
	zeroParam()
	return 42
}
func namedVoidBinding() int {
	zeroParam()
	u := struct{}{}
	_ = u
	fmt.Println(u)
	return 42
}
func voidLetNoContinuation() int {
	zeroParam()
	return 42
}
func callCoercedCallbacks() {
	f := getCoercedCallback()
	_ = f
	f()
	g := getCoercedCallbackUnit()
	_ = g
	g()
}
func main() {
	zeroParam()
	structArg(struct{}{})
	bindAndPass()
	passDirect()
	zeroParamStruct()
	bindStructResult()
	withStructCallback()
	withNamedStructCallback()
	withZeroCallback()
	withNamedZeroCallback()
	unitReturnCoercedToStruct()
	structReturnCoercedToUnit()
	unitReturnCoercedToStructI(1)
	structReturnCoercedToUnitI(2)
	multiUnitParams(struct{}{}, struct{}{}, struct{}{})
	applyMultiUnit(multiUnitParams)
	fmt.Println(discardUnit)
	localCoerceToStruct()
	localCoerceToUnit()
	localCoerceParamToStruct()
	localCoerceParamToUnit()
	argCoerceToStruct()
	argCoerceToUnit()
	getCoercedCallback()
	getCoercedCallbackUnit()
	fmt.Println(voidIfInExpr())
	fmt.Println(voidMatchInExpr(0))
	fmt.Println(voidSeqInExpr())
	fmt.Println(namedVoidBinding())
	fmt.Println(voidLetNoContinuation())
	callCoercedCallbacks()
}
