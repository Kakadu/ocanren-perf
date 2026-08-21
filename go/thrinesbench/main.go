// Command thrinesbench is the Go port of the C++ thrines benchmark: synthesize
// two thrines (triples p, q, r of pairwise-different terms that evaluate to one
// another in a 3-cycle) and time it. Go goroutine stacks grow on demand, so no
// large-stack worker thread is needed (unlike the C++ port).
package main

import (
	"fmt"
	"os"
	"strconv"
	"strings"
	"time"

	"ocanren-go/mk"
	"ocanren-go/mk/thr"
)

// peakRSSBytes reads VmHWM from /proc/self/status (Linux), in bytes.
func peakRSSBytes() int64 {
	data, err := os.ReadFile("/proc/self/status")
	if err != nil {
		return -1
	}
	for _, line := range strings.Split(string(data), "\n") {
		if strings.HasPrefix(line, "VmHWM:") {
			f := strings.Fields(line)
			if len(f) >= 2 {
				v, _ := strconv.ParseInt(f[1], 10, 64)
				return v * 1024
			}
		}
	}
	return -1
}

// findThrines runs the thrines search taking n answers; prints each when verbose.
func findThrines(n int, verbose bool) {
	st := mk.EmptyState()
	q := mk.MkVar(st.Env().Fresh())
	states := mk.Take(n, thr.Thrineso(q)(st))
	for _, s := range states {
		if !verbose {
			continue
		}
		triple := mk.Reify(s.Subst(), q) // (p, (q, r))
		p := triple.Car()
		qq := triple.Cdr().Car()
		rr := triple.Cdr().Cdr()
		fmt.Printf("* %s\n  %s\n  %s\n\n", thr.ShowGterm(p), thr.ShowGterm(qq), thr.ShowGterm(rr))
	}
}

func timeFind(n int) float64 {
	t0 := time.Now()
	findThrines(n, false)
	return time.Since(t0).Seconds()
}

func main() {
	repeat := 1
	if e := os.Getenv("REPEAT"); e != "" {
		if v, err := strconv.Atoi(e); err == nil && v >= 1 {
			repeat = v
		}
	}

	// Show the two thrines once for verification.
	findThrines(2, true)

	// Warmup (not timed), then REPEAT timed runs -- like TimeHelper.wrap.
	findThrines(2, false)
	var acc float64
	for i := 0; i < repeat; i++ {
		acc += timeFind(2)
	}
	avg := acc / float64(repeat)

	fmt.Printf("avg time to find 2 thrines: %.6fs over %d run(s)\n", avg, repeat)

	// Term-arena memory (cumulative over all searches in this process).
	nterms := mk.Terms().Size()
	soaBytes := mk.Terms().Bytes()
	aosBytes := nterms * 56 // old array-of-structs node size
	fmt.Printf("terms allocated: %d\n", nterms)
	fmt.Printf("term arena (SoA): %.2f MB   (old AoS would be %.2f MB)\n",
		float64(soaBytes)/(1024*1024), float64(aosBytes)/(1024*1024))
	if rss := peakRSSBytes(); rss > 0 {
		fmt.Printf("peak RSS: %.2f MB\n", float64(rss)/(1024*1024))
	}
}
