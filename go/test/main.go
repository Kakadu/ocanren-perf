// Command test is the Go port of the C++ test suite in ../cpp/test/main.cpp:
// the same queries and expected output, mirroring OCanren's own regression
// tests (test001 appendo/reverso, test005-007 STLC) plus the disequality
// checks. It runs every check and reports "OK: all N checks passed".
package main

import (
	"fmt"
	"strings"

	"ocanren-go/mk"
)

var checks, failures int

func check(cond bool, what string) {
	checks++
	if !cond {
		fmt.Printf("FAIL: %s\n", what)
		failures++
	}
}

func checkEq(got, want, what string) {
	checks++
	if got != want {
		fmt.Printf("FAIL: %s\n  got:  %s\n  want: %s\n", what, got, want)
		failures++
	}
}

// norm replaces every free-variable index (_.123) with _.? so a check can
// verify the *structure* of a reified term without depending on the exact
// fresh-variable allocation order.
func isDigit(c byte) bool { return c >= '0' && c <= '9' }

func norm(s string) string {
	var out strings.Builder
	i := 0
	for i < len(s) {
		if s[i] == '_' && i+2 < len(s) && s[i+1] == '.' && isDigit(s[i+2]) {
			out.WriteString("_.?")
			i += 2
			for i < len(s) && isDigit(s[i]) {
				i++
			}
		} else {
			out.WriteByte(s[i])
			i++
		}
	}
	return out.String()
}

func checkEqNorm(got, want, what string) { checkEq(norm(got), norm(want), what) }

// --- answer helpers ------------------------------------------------------

// answers: single query variable, reified.
func answers(query func(mk.Term) mk.Goal, n int) []mk.Term {
	return mk.Take(n, mk.Run(query))
}

// answers2: two query variables, reified together.
func answers2(query func(mk.Term, mk.Term) mk.Goal, n int) [][2]mk.Term {
	st := mk.EmptyState()
	q := mk.MkVar(st.Env().Fresh())
	r := mk.MkVar(st.Env().Fresh())
	states := mk.Take(n, query(q, r)(st))
	out := make([][2]mk.Term, 0, len(states))
	for _, s := range states {
		out = append(out, [2]mk.Term{mk.Reify(s.Subst(), q), mk.Reify(s.Subst(), r)})
	}
	return out
}

// answers3: three query variables, reified together.
func answers3(query func(mk.Term, mk.Term, mk.Term) mk.Goal, n int) [][3]mk.Term {
	st := mk.EmptyState()
	q := mk.MkVar(st.Env().Fresh())
	r := mk.MkVar(st.Env().Fresh())
	s := mk.MkVar(st.Env().Fresh())
	states := mk.Take(n, query(q, r, s)(st))
	out := make([][3]mk.Term, 0, len(states))
	for _, stt := range states {
		out = append(out, [3]mk.Term{mk.Reify(stt.Subst(), q), mk.Reify(stt.Subst(), r), mk.Reify(stt.Subst(), s)})
	}
	return out
}

func main() {
	testCowSubstitution()
	testArena()
	testAppendoForward()
	testAppendoBackward()
	testAppendoSplits()
	testAppendoFree()
	testReversoForward()
	testReversoBackward()
	testReversoSingleton()
	testReversoEmpty()
	testReversoPalindromes()
	testAAndB()
	testAAndBPrime()
	testSuccess()
	testFreshSingletonList()
	testFreshPair()
	testTwoVars()
	testReversoForwardSingleton()
	testOccursCheck()
	testFives()
	testDiseqFresh()
	testDiseqViolatedByUnify()
	testDiseqViolatedGround()
	testDiseqFulfilled()
	testDiseqPrunesBranch()
	testDiseqThreeDistinct()
	testDiseqListTerm()
	test005LookupoEmpty()
	test005LookupoSingle()
	test005LookupoTwo()
	test005LookupoFindX()
	test005LookupoFindY()
	test005InferoUntypable()
	test005LookupoFree()
	test005InferoId()
	test005InferoCompose()
	test005InferoFlip()
	test005InferoFromType()
	test006Substo()
	test006EvaloAbs()
	test006EvaloApp()
	test006EvaloAppFreeArg()
	test006EvaloAppFreeBody()
	test006EvaloAppVar()
	test006EvaloVar()
	test006EvaloFreeFun()
	test006EvaloFreeFunArg()
	test006ALaQuine()
	test007Substo()
	test007EvaloAbs()
	test007EvaloApp()
	test007EvaloAppFreeArg()
	test007EvaloAppFreeBody()
	test007EvaloAppVar()
	test007EvaloVar()
	test007EvaloFreeFun()
	test007EvaloFreeFunArg()

	if failures > 0 {
		fmt.Printf("FAILED: %d of %d checks failed\n", failures, checks)
		return
	}
	fmt.Printf("OK: all %d checks passed\n", checks)
}
