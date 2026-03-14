package main

import "fmt"

func isLeapYear(year any) any {
	if ((year % 400) == 0) {
		return true
	} else if ((year % 100) == 0) {
		return false
	} else {
		return ((year % 4) == 0)
	}
}
