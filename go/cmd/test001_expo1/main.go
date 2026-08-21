// Command test001_expo1 is the Go port of ocanren01/test001_expo1.ml:
//
//	expo (build_num 3) (build_num 5) q
//
// It synthesizes the base-? numeral for 3^5 and reports the unification count
// and timing (REPEAT=N to repeat, like OCanren's TimeHelper.wrap).
package main

import (
	"fmt"

	"ocanren-go/bench"
)

func main() {
	repeat := bench.Repeat()
	avg, count := bench.TimeAndCount(repeat, func() int64 {
		c, _ := bench.Expo(3, 5, 1)
		return c
	})
	fmt.Println("test001_expo1: expo (build_num 3) (build_num 5) q")
	fmt.Printf("  unifications: %d\n", count)
	fmt.Printf("  avg time: %.6fs over %d run(s)\n", avg, repeat)
}
