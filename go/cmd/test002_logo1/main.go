// Command test002_logo1 is the Go port of ocanren01/test002_logo1.ml:
//
//	logo (build_num 243) (build_num 3) q (build_num 0)
//
// It synthesizes the base-3 numeral for 243 and reports the unification count
// and timing (REPEAT=N to repeat, like OCanren's TimeHelper.wrap).
package main

import (
	"fmt"

	"ocanren-go/bench"
)

func main() {
	repeat := bench.Repeat()
	avg, count := bench.TimeAndCount(repeat, func() int64 {
		c, _ := bench.Logo(243, 3, 1)
		return c
	})
	fmt.Println("test002_logo1: logo (build_num 243) (build_num 3) q (build_num 0)")
	fmt.Printf("  unifications: %d\n", count)
	fmt.Printf("  avg time: %.6fs over %d run(s)\n", avg, repeat)
}
