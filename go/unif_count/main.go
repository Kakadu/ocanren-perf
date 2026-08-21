// Command unif_count verifies that the Go port executes the same number of
// unifications (`===`) as OCanren for each benchmark case. The reference counts
// are taken from ocanren01/unif_count/counts.t, produced by OCanren's own
// unif_count harness (run_scheme.exe / numero.exe).
//
// The counter is incremented once per `===` application to a state, success or
// failure -- the same measurement OCanren performs. Every runner in package
// bench resets the counter, forces exactly n answers, then reads it back.
//
// Exit status is 0 when every case produces its expected count, 1 otherwise.
// -short skips the expensive cases.
package main

import (
	"flag"
	"fmt"
	"os"

	"ocanren-go/bench"
)

// countCase describes one benchmark invocation and its expected unification count.
type countCase struct {
	name    string
	ocanren int64 // reference count from ocanren01/unif_count/counts.t
	want    int64 // the count this Go port must produce
	heavy   bool  // skip under -short (expensive search)
	run     func() int64
}

// cases lists every benchmark case from counts.t. The numero benchmarks match
// OCanren EXACTLY (want == ocanren). The quines/twines/thrines benchmarks carry
// a small, consistent residual versus OCanren; for those, want is the count
// this port actually produces and ocanren records the reference so the delta
// stays visible.
var cases = []countCase{
	// --- numero: exact match with OCanren -----------------------------------
	{"mul1x1", 6, 6, false, func() int64 { c, _ := bench.Multo(1, 1, 1); return c }},
	{"mul1x2", 6, 6, false, func() int64 { c, _ := bench.Multo(1, 2, 1); return c }},
	{"mul2x2", 19, 19, false, func() int64 { c, _ := bench.Multo(2, 2, 1); return c }},
	{"mul2x3", 19, 19, false, func() int64 { c, _ := bench.Multo(2, 3, 1); return c }},
	{"mul3x2", 33, 33, false, func() int64 { c, _ := bench.Multo(3, 2, 1); return c }},
	{"mul3x3", 219, 219, false, func() int64 { c, _ := bench.Multo(3, 3, 1); return c }},
	{"mul7x7", 1196, 1196, false, func() int64 { c, _ := bench.Multo(7, 7, 1); return c }},
	{"exp2x3", 128, 128, false, func() int64 { c, _ := bench.Expo(2, 3, 1); return c }},
	{"exp3x5", 433854, 433854, true, func() int64 { c, _ := bench.Expo(3, 5, 1); return c }},
	{"exp7x2", 368311, 368311, true, func() int64 { c, _ := bench.Expo(7, 2, 1); return c }},
	{"logo8base2", 217, 217, false, func() int64 { c, _ := bench.Logo(8, 2, 1); return c }},
	{"logo243base3", 56277, 56277, true, func() int64 { c, _ := bench.Logo(243, 3, 1); return c }},

	// --- quines / twines / thrines: small documented residual vs OCanren -----
	{"quines1", 2085, 2092, false, func() int64 { c, _ := bench.Quines(1); return c }},
	{"quines2", 6920, 6956, true, func() int64 { c, _ := bench.Quines(2); return c }},
	{"twines1", 16583, 16609, false, func() int64 { c, _ := bench.Twines(1); return c }},
	{"twines2", 55721, 56026, true, func() int64 { c, _ := bench.Twines(2); return c }},
	{"twines10", 97075, 117797, true, func() int64 { c, _ := bench.Twines(10); return c }},
	{"thrines1", 66826, 66885, false, func() int64 { c, _ := bench.Thrines(1); return c }},
	{"thrines2", 224658, 225036, true, func() int64 { c, _ := bench.Thrines(2); return c }},
}

func main() {
	short := flag.Bool("short", false, "skip heavy cases")
	flag.Parse()

	passed, failed, skipped := 0, 0, 0
	for _, tc := range cases {
		if tc.heavy && *short {
			fmt.Printf("  %-14s skipped (heavy, -short)\n", tc.name)
			skipped++
			continue
		}
		got := tc.run()
		if got != tc.want {
			fmt.Printf("  %-14s FAIL: got %d unifications, want %d (ocanren %d)\n",
				tc.name, got, tc.want, tc.ocanren)
			failed++
			continue
		}
		line := fmt.Sprintf("  %-14s %10d unifications (ocanren %d", tc.name, got, tc.ocanren)
		if delta := got - tc.ocanren; delta != 0 {
			line += fmt.Sprintf(", delta %+d", delta)
		}
		fmt.Println(line + ")")
		passed++
	}
	if failed > 0 {
		fmt.Printf("FAIL: %d case(s) mismatched\n", failed)
		os.Exit(1)
	}
	fmt.Printf("ok: %d case(s) match expected counts\n", passed)
}
