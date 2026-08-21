// Command test005_thrines is the Go port of ocanren01/test005_thrines.ml:
//
//	find_thrines 2
//
// It synthesizes two thrines (triples p, q, r of pairwise-different terms that
// evaluate to one another in a 3-cycle) and reports the unification count and
// timing (REPEAT=N to repeat, like OCanren's TimeHelper.wrap).
package main

import (
	"fmt"

	"ocanren-go/bench"
)

func main() {
	repeat := bench.Repeat()
	avg, count := bench.TimeAndCount(repeat, func() int64 {
		c, _ := bench.Thrines(2)
		return c
	})
	fmt.Println("test005_thrines: find_thrines 2")
	fmt.Printf("  unifications: %d\n", count)
	fmt.Printf("  avg time: %.6fs over %d run(s)\n", avg, repeat)
}
