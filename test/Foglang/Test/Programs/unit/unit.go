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

var discardUnit int = func() int { zeroParam(); return 42 }()

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
}
