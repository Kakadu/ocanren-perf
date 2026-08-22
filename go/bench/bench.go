// Package bench runs the five OCanren benchmarks (and the numero count cases)
// and reports their unification counts. Every runner resets the global
// unification counter, forces exactly n answers, then reads the counter -- the
// same measurement OCanren's unif_count harness performs.
package bench

import (
	"os"
	"strconv"
	"time"

	"ocanren-go/mk"
	"ocanren-go/mk/nodiseq"
	"ocanren-go/mk/numero"
	"ocanren-go/mk/stdx"
	"ocanren-go/mk/thr"
)

// Repeat returns the REPEAT env var (default 1), like OCanren's TimeHelper.
func Repeat() int {
	if e := os.Getenv("REPEAT"); e != "" {
		if v, err := strconv.Atoi(e); err == nil && v >= 1 {
			return v
		}
	}
	return 1
}

// run1 runs a single-query-variable relation taking n answers; returns the
// unification count and the reified answers.
func run1(n int, query func(mk.Term) mk.Goal) (int64, []mk.Term) {
	mk.ResetUnifications()
	st := mk.EmptyState()
	q := mk.MkVar(st.Env().Fresh())
	states := mk.Take(n, query(q)(st))
	answers := make([]mk.Term, 0, len(states))
	for _, s := range states {
		answers = append(answers, mk.Reify(s.Subst(), q))
	}
	return mk.UnificationCount(), answers
}

// Expo runs `expo (build_num b) (build_num e) q`, taking n answers.
func Expo(b, e int64, n int) (int64, []mk.Term) {
	return run1(n, func(q mk.Term) mk.Goal {
		return numero.Expo(numero.BuildNum(b), numero.BuildNum(e), q)
	})
}

// Logo runs `logo (build_num n) (build_num b) q (build_num 0)`, taking n answers.
func Logo(n, b int64, take int) (int64, []mk.Term) {
	return run1(take, func(q mk.Term) mk.Goal {
		return numero.Logo(numero.BuildNum(n), numero.BuildNum(b), q, numero.Nil())
	})
}

// Multo runs `multo (build_num a) (build_num b) q`, taking n answers.
func Multo(a, b int64, take int) (int64, []mk.Term) {
	return run1(take, func(q mk.Term) mk.Goal {
		return numero.Multo(numero.BuildNum(a), numero.BuildNum(b), q)
	})
}

// Appendo runs `appendo [xs] [ys] q`, taking n answers.
func Appendo(xs, ys []int64, take int) (int64, []mk.Term) {
	return run1(take, func(q mk.Term) mk.Goal {
		return stdx.Appendo(stdx.List(xs...), stdx.List(ys...), q)
	})
}

// Reverso runs `reverso [xs] q`, taking n answers.
func Reverso(xs []int64, take int) (int64, []mk.Term) {
	return run1(take, func(q mk.Term) mk.Goal {
		return stdx.Reverso(stdx.List(xs...), q)
	})
}

// Plus runs `pluso (build_num a) (build_num b) q`, taking n answers.
func Plus(a, b int64, take int) (int64, []mk.Term) {
	return run1(take, func(q mk.Term) mk.Goal {
		return numero.Pluso(numero.BuildNum(a), numero.BuildNum(b), q)
	})
}

// Addero runs `addero (build_num d) (build_num a) (build_num b) q`, taking n answers.
func Addero(d, a, b int64, take int) (int64, []mk.Term) {
	return run1(take, func(q mk.Term) mk.Goal {
		return numero.Addero(numero.BuildNum(d), numero.BuildNum(a), numero.BuildNum(b), q)
	})
}

// Quines runs `find_quines n`.
func Quines(n int) (int64, []mk.Term) {
	return run1(n, func(q mk.Term) mk.Goal { return thr.Quineso(q) })
}

// QuinesNoDiseq runs the nodiseq `find_quines n`: the same quine search but with
// a peano-indexed interpreter whose only disequality is the relational `neq`, so
// it uses pure unification (no built-in disequality). The produced quine matches
// OCanren's; the count runs higher due to the scheduler difference (see AGENTS.md).
func QuinesNoDiseq(n int) (int64, []mk.Term) {
	return run1(n, func(q mk.Term) mk.Goal { return nodiseq.Quineo(q) })
}

// Thrines runs `find_thrines n`.
func Thrines(n int) (int64, []mk.Term) {
	return run1(n, func(x mk.Term) mk.Goal { return thr.Thrineso(x) })
}

// Twines runs `find_twines n`: two query variables q, p with the goal
// `twineso q p`. The variables are created in OCanren's order (q first) and
// reified separately -- no extra pair-unification.
func Twines(n int) (int64, []mk.Term) {
	mk.ResetUnifications()
	st := mk.EmptyState()
	q := mk.MkVar(st.Env().Fresh()) // created first (smaller index)
	p := mk.MkVar(st.Env().Fresh())
	states := mk.Take(n, thr.Twineso(q, p)(st))
	answers := make([]mk.Term, 0, len(states))
	for _, s := range states {
		answers = append(answers, mk.MkPair(mk.Reify(s.Subst(), q), mk.Reify(s.Subst(), p)))
	}
	return mk.UnificationCount(), answers
}

// TimeRepeat runs f once as a warmup (untimed), then repeat timed runs, and
// returns the average seconds -- mirroring OCanren's TimeHelper.wrap.
func TimeRepeat(repeat int, f func()) float64 {
	f() // warmup
	var acc float64
	for i := 0; i < repeat; i++ {
		t0 := time.Now()
		f()
		acc += time.Since(t0).Seconds()
	}
	return acc / float64(repeat)
}

// TimeAndCount runs f (one full benchmark, returning its unification count)
// once as a warmup, then repeat timed runs. It returns the average seconds and
// the count from the last run -- the measurement each named benchmark reports.
func TimeAndCount(repeat int, f func() int64) (float64, int64) {
	var count int64
	f() // warmup
	var acc float64
	for i := 0; i < repeat; i++ {
		t0 := time.Now()
		count = f()
		acc += time.Since(t0).Seconds()
	}
	return acc / float64(repeat), count
}
