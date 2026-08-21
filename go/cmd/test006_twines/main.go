// Command test006_twines is the Go port of ocanren01/test006_twines.ml:
//
//	find_twines 30
//
// It synthesizes 30 twines (pairs q, p of different terms that evaluate to one
// another) and reports the unification count and timing (REPEAT=N to repeat,
// like OCanren's TimeHelper.wrap). This is a heavy search; use REPEAT=1.
package main

import (
	"fmt"

	"ocanren-go/bench"
)

func main() {
	repeat := bench.Repeat()
	avg, count := bench.TimeAndCount(repeat, func() int64 {
		c, _ := bench.Twines(30)
		return c
	})
	fmt.Println("test006_twines: find_twines 30")
	fmt.Printf("  unifications: %d\n", count)
	fmt.Printf("  avg time: %.6fs over %d run(s)\n", avg, repeat)
}
