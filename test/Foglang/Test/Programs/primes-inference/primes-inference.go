package main

import "fmt"

func trialDivision(count int) []int {
	var filterInts func(pred func(int) bool, xs []int) []int
	filterInts = func(pred func(int) bool, xs []int) []int {
		_scrut2 := xs
		_ = _scrut2
		if len(_scrut2) == 0 {
			return []int{}
		} else if len(_scrut2) > 0 {
			x := _scrut2[0]
			_ = x
			rest := _scrut2[1:]
			_ = rest
			if pred(x) {
				return append([]int{x}, filterInts(pred, rest)...)
			} else {
				return filterInts(pred, rest)
			}
		}
		panic("match not exhaustive")
	}
	_ = filterInts
	var sieve func(n int, candidates []int) []int
	sieve = func(n int, candidates []int) []int {
		if n == 0 {
			return []int{}
		} else {
			_scrut3 := candidates
			_ = _scrut3
			if len(_scrut3) == 0 {
				return []int{}
			} else if len(_scrut3) > 0 {
				x := _scrut3[0]
				_ = x
				rest := _scrut3[1:]
				_ = rest
				return append([]int{x}, sieve((n-1), filterInts(func(y int) bool { return ((y % x) != 0) }, rest))...)
			}
			panic("match not exhaustive")
		}
	}
	_ = sieve
	return sieve(count, intRange(2, 1, 200000))
}
func primes(count int) []int {
	var nextOddMultiple func(after int, p int) int
	nextOddMultiple = func(after int, p int) int {
		candidate := (after + (2 * p))
		_ = candidate
		if (candidate % 2) != 0 {
			return candidate
		} else {
			return (after + (3 * p))
		}
	}
	_ = nextOddMultiple
	var firstOddMultiple func(p int) int
	firstOddMultiple = func(p int) int {
		return nextOddMultiple(0, p)
	}
	_ = firstOddMultiple
	var addFactor func(table map[int][]int, composite int, prime int) map[int][]int
	addFactor = func(table map[int][]int, composite int, prime int) map[int][]int {
		_scrut2_0, _scrut2_1 := table[composite]
		_ = _scrut2_0
		_ = _scrut2_1
		if _scrut2_1 {
			existing := _scrut2_0
			_ = existing
			return mapInsert(table, composite, append(existing, prime))
		} else if !(_scrut2_1) {
			return mapInsert(table, composite, []int{prime})
		}
		panic("match not exhaustive")
	}
	_ = addFactor
	var foldFactors func(f func(map[int][]int, int) map[int][]int, acc map[int][]int, xs []int) map[int][]int
	foldFactors = func(f func(map[int][]int, int) map[int][]int, acc map[int][]int, xs []int) map[int][]int {
		_scrut2 := xs
		_ = _scrut2
		if len(_scrut2) == 0 {
			return acc
		} else if len(_scrut2) > 0 {
			x := _scrut2[0]
			_ = x
			rest := _scrut2[1:]
			_ = rest
			return foldFactors(f, f(acc, x), rest)
		}
		panic("match not exhaustive")
	}
	_ = foldFactors
	var sieve func(n int, candidates []int, table map[int][]int) []int
	sieve = func(n int, candidates []int, table map[int][]int) []int {
		if n == 0 {
			return []int{}
		} else {
			_scrut3 := candidates
			_ = _scrut3
			if len(_scrut3) == 0 {
				return []int{}
			} else if len(_scrut3) > 0 {
				x := _scrut3[0]
				_ = x
				rest := _scrut3[1:]
				_ = rest
				_scrut4_0, _scrut4_1 := table[x]
				_ = _scrut4_0
				_ = _scrut4_1
				if !(_scrut4_1) {
					return append([]int{x}, sieve((n-1), rest, addFactor(table, firstOddMultiple(x), x))...)
				} else if _scrut4_1 {
					factors := _scrut4_0
					_ = factors
					table_shadow1 := foldFactors(func(m map[int][]int, p int) map[int][]int { return addFactor(m, nextOddMultiple(x, p), p) }, mapDelete(table, x), factors)
					_ = table_shadow1
					return sieve(n, rest, table_shadow1)
				}
				panic("match not exhaustive")
			}
			panic("match not exhaustive")
		}
	}
	_ = sieve
	if count < 1 {
		return []int{}
	} else {
		return append([]int{2}, sieve((count-1), intRange(3, 2, 200000), map[int][]int{})...)
	}
}
func main() {
	fast := primes(10000)
	_ = fast
	fmt.Println("genuine sieve:")
	fmt.Println(len(fast))
	fmt.Println(fast[(len(fast) - 1)])
}
