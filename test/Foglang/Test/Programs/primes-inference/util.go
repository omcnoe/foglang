package main

// Support functions for primes.fog that can't yet be expressed in fog.
// These will be removed as fog gains the necessary language features.

// mapInsert returns a new map with key set to value.
func mapInsert(m map[int][]int, key int, value []int) map[int][]int {
	m2 := make(map[int][]int, len(m)+1)
	for k, v := range m {
		m2[k] = v
	}
	m2[key] = value
	return m2
}

// mapDelete returns a new map with key removed.
func mapDelete(m map[int][]int, key int) map[int][]int {
	m2 := make(map[int][]int, len(m))
	for k, v := range m {
		m2[k] = v
	}
	delete(m2, key)
	return m2
}

// intRange returns integers from start to limit, stepping by step.
func intRange(start, step, limit int) []int {
	var result []int
	for i := start; i <= limit; i += step {
		result = append(result, i)
	}
	return result
}
