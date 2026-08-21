package main

import (
	"ocanren-go/mk"
	stdx "ocanren-go/mk/stdx"
)

// --- copy-on-write substitution -----------------------------------------

func testCowSubstitution() {
	s0 := mk.EmptySubst()
	a := mk.Var{Env: 1, Index: 10}
	b := mk.Var{Env: 1, Index: 11}
	s1 := s0.Extend(a, mk.MkInt(1))
	s2 := s1.Extend(b, mk.MkInt(2))

	check(s0.IsEmpty(), "s0 is empty")
	check(s1.Lookup(a) != mk.NoTerm && s1.Lookup(b) == mk.NoTerm, "s1 binds only a")
	check(s2.Lookup(a) != mk.NoTerm && s2.Lookup(b) != mk.NoTerm, "s2 binds a and b")
	check(s1.Lookup(b) == mk.NoTerm, "s1 is unaffected by extend (copy-on-write)")
	check(s0.Lookup(a) == mk.NoTerm, "s0 is unaffected by extend")
	check(s2.Size() == 2, "s2 has size 2")

	// Replacing an existing binding (OCaml Map.add semantics).
	s3 := s2.Extend(a, mk.MkInt(42))
	check(s3.Size() == 2, "extend replaces an existing key")
	check(s2.Lookup(a).IntVal() == 1, "old version still has the old binding")
}

// --- arena ----------------------------------------------------------------

func testArena() {
	t := mk.MkCons(mk.MkInt(1), mk.MkCons(mk.MkInt(2), mk.MkNil()))
	u := mk.MkCons(mk.MkInt(1), mk.MkCons(mk.MkInt(2), mk.MkNil()))
	check(t.Kind() == mk.KCons, "arena term is a cons")
	check(t.Car().IntVal() == 1 && t.Cdr().Cdr().Kind() == mk.KNil, "arena term contents")
	check(t != u, "distinct nodes are allocated")
	s := mk.MkStr("hello")
	check(s.StrVal() == "hello", "arena string")
	v := mk.Var{Env: 1, Index: 10}
	x := mk.MkVar(v)
	check(x.Kind() == mk.KVar && x.VarVal().Equal(v), "arena var")
}

// --- appendo --------------------------------------------------------------

func testAppendoForward() {
	ans := answers(func(q mk.Term) mk.Goal { return stdx.Appendo(stdx.List(1, 2), stdx.List(3, 4), q) }, 10)
	check(len(ans) == 1, "appendo [1,2] [3,4] q: 1 answer")
	checkEq(mk.Show(ans[0]), "[1, 2, 3, 4]", "appendo [1,2] [3,4] q")
}

func testAppendoBackward() {
	ans := answers(func(q mk.Term) mk.Goal { return stdx.Appendo(q, stdx.List(3, 4), stdx.List(1, 2, 3, 4)) }, 10)
	check(len(ans) == 1, "appendo q [3,4] [1,2,3,4]: 1 answer")
	checkEq(mk.Show(ans[0]), "[1, 2]", "appendo q [3,4] [1,2,3,4]")
}

func testAppendoSplits() {
	ans := answers2(func(q, r mk.Term) mk.Goal { return stdx.Appendo(q, r, stdx.List(1, 2)) }, 10)
	check(len(ans) == 3, "appendo q r [1,2]: 3 answers")
	checkEq(mk.Show(ans[0][0])+" "+mk.Show(ans[0][1]), "[] [1, 2]", "split 1")
	checkEq(mk.Show(ans[1][0])+" "+mk.Show(ans[1][1]), "[1] [2]", "split 2")
	checkEq(mk.Show(ans[2][0])+" "+mk.Show(ans[2][1]), "[1, 2] []", "split 3")
}

func testAppendoFree() {
	// appendo q [] r -- an infinite stream of answers with q = r.
	ans := answers2(func(q, r mk.Term) mk.Goal { return stdx.Appendo(q, stdx.Nil(), r) }, 4)
	check(len(ans) == 4, "appendo q [] r: 4 answers taken")
	checkEq(mk.Show(ans[0][0]), "[]", "q[0]")
	checkEq(mk.Show(ans[1][0]), "[_.12]", "q[1]")
	checkEq(mk.Show(ans[2][0]), "[_.12, _.15]", "q[2]")
	checkEq(mk.Show(ans[3][0]), "[_.12, _.15, _.18]", "q[3]")
	for _, pr := range ans {
		check(mk.Show(pr[0]) == mk.Show(pr[1]), "q == r")
	}
}

// --- reverso --------------------------------------------------------------

func testReversoForward() {
	// One answer only; the stream tail after it is an infinite chain of
	// failing branches (the same in OCanren), so take exactly one answer.
	ans := answers(func(q mk.Term) mk.Goal { return stdx.Reverso(stdx.List(1, 2, 3, 4), q) }, 1)
	check(len(ans) == 1, "reverso [1,2,3,4] q: 1 answer")
	checkEq(mk.Show(ans[0]), "[4, 3, 2, 1]", "reverso [1,2,3,4] q")
}

func testReversoBackward() {
	ans := answers(func(q mk.Term) mk.Goal { return stdx.Reverso(q, stdx.List(1, 2, 3, 4)) }, 1)
	check(len(ans) == 1, "reverso q [1,2,3,4]: 1 answer")
	checkEq(mk.Show(ans[0]), "[4, 3, 2, 1]", "reverso q [1,2,3,4]")
}

func testReversoSingleton() {
	ans := answers(func(q mk.Term) mk.Goal { return stdx.Reverso(q, stdx.List(1)) }, 2)
	check(len(ans) == 1, "reverso q [1]: 1 answer")
	checkEq(mk.Show(ans[0]), "[1]", "reverso q [1]")
}

func testReversoEmpty() {
	ans := answers(func(mk.Term) mk.Goal { return stdx.Reverso(stdx.Nil(), stdx.Nil()) }, 10)
	check(len(ans) == 1, "reverso [] []: 1 answer")
	checkEq(mk.Show(ans[0]), "_.10", "reverso [] [] leaves q free")
}

func testReversoPalindromes() {
	// The exact variable indices must match OCanren's test001.t, which is a
	// strong end-to-end check of the search order, laziness and the shared
	// fresh-variable counter (OCanren's Env.next is a mutable field shared
	// by all branches of a run).
	golden := []string{
		"[]",
		"[_.11]",
		"[_.23, _.23]",
		"[_.56, _.32, _.56]",
		"[_.110, _.89, _.89, _.110]",
		"[_.188, _.167, _.134, _.167, _.188]",
		"[_.293, _.266, _.236, _.236, _.266, _.293]",
		"[_.422, _.398, _.365, _.329, _.365, _.398, _.422]",
		"[_.578, _.554, _.524, _.488, _.488, _.524, _.554, _.578]",
		"[_.770, _.743, _.713, _.683, _.638, _.683, _.713, _.743, _.770]",
	}
	ans := answers(func(q mk.Term) mk.Goal { return stdx.Reverso(q, q) }, 10)
	check(len(ans) == 10, "reverso q q: 10 answers")
	for i := range ans {
		checkEq(mk.Show(ans[i]), golden[i], "reverso q q palindrome")
	}
}

// --- misc -----------------------------------------------------------------

// a_and_b q = fresh b (q === 7 &&& conde [b === 6; b === 5]) -- 1 answer.
func testAAndB() {
	ans := answers(func(q mk.Term) mk.Goal {
		return mk.Fresh(func(b mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(q, mk.MkInt(7)),
				mk.Conde([]mk.Goal{mk.UnifyGoal(b, mk.MkInt(6)), mk.UnifyGoal(b, mk.MkInt(5))}))
		})
	}, 1)
	check(len(ans) == 1, "a_and_b q: 1 answer")
	checkEq(mk.Show(ans[0]), "7", "a_and_b q")
}

// a_and_b' q = fresh a (7 === 7 &&& conde [q === 6; q === 5]) -- 2 answers.
func testAAndBPrime() {
	ans := answers(func(q mk.Term) mk.Goal {
		return mk.Fresh(func(mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(mk.MkInt(7), mk.MkInt(7)),
				mk.Conde([]mk.Goal{mk.UnifyGoal(q, mk.MkInt(6)), mk.UnifyGoal(q, mk.MkInt(5))}))
		})
	}, 10)
	check(len(ans) == 2, "a_and_b' q: 2 answers")
	checkEq(mk.Show(ans[0]), "6", "a_and_b' q[0]")
	checkEq(mk.Show(ans[1]), "5", "a_and_b' q[1]")
}

func testSuccess() {
	ans := answers(func(mk.Term) mk.Goal { return mk.Success }, 1)
	check(len(ans) == 1, "success: 1 answer")
	checkEq(mk.Show(ans[0]), "_.10", "success leaves q free")
}

func testFreshSingletonList() {
	ans := answers(func(q mk.Term) mk.Goal {
		return mk.Fresh(func(n mk.Term) mk.Goal { return mk.UnifyGoal(q, mk.MkCons(n, mk.MkNil())) })
	}, 1)
	check(len(ans) == 1, "fresh n (q === n % nil): 1 answer")
	checkEq(mk.Show(ans[0]), "[_.11]", "fresh n (q === n % nil)")
}

func testFreshPair() {
	ans := answers(func(q mk.Term) mk.Goal {
		return mk.Fresh3(func(a, b, c mk.Term) mk.Goal {
			return mk.UnifyGoal(q, mk.MkCons(mk.MkCons(a, b), c))
		})
	}, 1)
	check(len(ans) == 1, "fresh (a b c) (q === (a % b) % c): 1 answer")
	checkEq(mk.Show(ans[0]), "[[_.11 | _.12] | _.13]", "fresh (a b c) (q === (a % b) % c)")
}

func testTwoVars() {
	ans := answers2(func(q, r mk.Term) mk.Goal { return mk.UnifyGoal(q, r) }, 1)
	check(len(ans) == 1, "two_vars q r: 1 answer")
	checkEq(mk.Show(ans[0][0])+" "+mk.Show(ans[0][1]), "_.11 _.11", "two_vars q r")
}

func testReversoForwardSingleton() {
	ans := answers(func(q mk.Term) mk.Goal { return stdx.Reverso(stdx.List(1), q) }, 1)
	check(len(ans) == 1, "reverso [1] q: 1 answer")
	checkEq(mk.Show(ans[0]), "[1]", "reverso [1] q")
}

func testOccursCheck() {
	ans := answers(func(q mk.Term) mk.Goal {
		return mk.UnifyGoal(q, mk.MkCons(mk.MkInt(1), q))
	}, 10)
	check(len(ans) == 0, "occurs check rejects q = 1 % q")
}

func testFives() {
	var fives func(mk.Term) mk.Goal
	fives = func(x mk.Term) mk.Goal {
		return mk.Conde([]mk.Goal{
			mk.UnifyGoal(x, mk.MkInt(5)),
			mk.Defer(func() mk.Goal { return fives(x) }),
		})
	}
	ans := answers(fives, 10)
	check(len(ans) == 10, "fives: 10 answers")
	for _, t := range ans {
		check(mk.Show(t) == "5", "fives answer is 5")
	}
}
