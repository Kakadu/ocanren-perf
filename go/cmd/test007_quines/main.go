// Command test007_quines is the Go port of ocanren01/test007_quines.ml:
//
//	find_quines 200
//
// It synthesizes 200 quines (terms that evaluate to themselves in the empty
// environment) and reports the unification count and timing (REPEAT=N to
// repeat, like OCanren's TimeHelper.wrap). This is a heavy search; use REPEAT=1.
package main

import (
	"fmt"

	"ocanren-go/bench"
)

func main() {
	repeat := bench.Repeat()
	avg, count := bench.TimeAndCount(repeat, func() int64 {
		c, _ := bench.Quines(200)
		return c
	})
	fmt.Println("test007_quines: find_quines 200")
	fmt.Printf("  unifications: %d\n", count)
	fmt.Printf("  avg time: %.6fs over %d run(s)\n", avg, repeat)
}
