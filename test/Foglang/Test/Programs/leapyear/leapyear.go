package main

import "fmt"

func isLeapYear(year int) bool {
	if (year % 400) == 0 {
		return true
	} else if (year % 100) == 0 {
		return false
	} else {
		return ((year % 4) == 0)
	}
}
func main() {
	fmt.Println(isLeapYear(2000))
	fmt.Println(isLeapYear(2004))
}
