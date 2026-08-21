package main

import "ocanren-go/mk"

// --- disequality (=/=) ----------------------------------------------------

func testDiseqFresh() {
	ans := answers2(func(q, r mk.Term) mk.Goal { return mk.DiseqGoal(q, r) }, 10)
	check(len(ans) == 1, "x =/= y: 1 answer")
	checkEq(mk.Show(ans[0][0])+" "+mk.Show(ans[0][1]), "_.10 _.11", "x =/= y both free")
}

func testDiseqViolatedByUnify() {
	ans := answers2(func(q, r mk.Term) mk.Goal {
		return mk.Conj(mk.UnifyGoal(q, r), mk.DiseqGoal(q, r))
	}, 10)
	check(len(ans) == 0, "x===y &&& x=/=y: no answer")
}

func testDiseqViolatedGround() {
	ans := answers(func(q mk.Term) mk.Goal {
		return mk.Conj(mk.DiseqGoal(q, mk.MkInt(1)), mk.UnifyGoal(q, mk.MkInt(1)))
	}, 10)
	check(len(ans) == 0, "x=/=1 &&& x===1: no answer")
}

func testDiseqFulfilled() {
	ans := answers(func(q mk.Term) mk.Goal {
		return mk.Conj(mk.DiseqGoal(q, mk.MkInt(1)), mk.UnifyGoal(q, mk.MkInt(2)))
	}, 10)
	check(len(ans) == 1, "x=/=1 &&& x===2: 1 answer")
	checkEq(mk.Show(ans[0]), "2", "x=/=1 &&& x===2 gives x=2")
}

func testDiseqPrunesBranch() {
	ans := answers(func(q mk.Term) mk.Goal {
		return mk.Fresh(func(x mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(q, x),
				mk.Conj(mk.DiseqGoal(x, mk.MkInt(1)),
					mk.Conde([]mk.Goal{mk.UnifyGoal(x, mk.MkInt(1)), mk.UnifyGoal(x, mk.MkInt(2))})))
		})
	}, 10)
	check(len(ans) == 1, "x=/=1 &&& conde[x===1;x===2]: 1 answer")
	checkEq(mk.Show(ans[0]), "2", "disequality keeps only x=2")
}

func testDiseqThreeDistinct() {
	ans := answers3(func(q, r, s mk.Term) mk.Goal {
		return mk.Conj(mk.Conj(mk.DiseqGoal(q, r), mk.DiseqGoal(r, s)), mk.DiseqGoal(s, q))
	}, 1)
	check(len(ans) == 1, "three pairwise-different vars: 1 answer")
	checkEq(mk.Show(ans[0][0])+" "+mk.Show(ans[0][1])+" "+mk.Show(ans[0][2]),
		"_.10 _.11 _.12", "three distinct free vars")
}

func testDiseqListTerm() {
	ans := answers(func(q mk.Term) mk.Goal {
		return mk.Fresh(func(a mk.Term) mk.Goal {
			return mk.Conj(mk.DiseqGoal(q, mk.MkCons(mk.MkInt(1), a)),
				mk.UnifyGoal(q, mk.MkCons(mk.MkInt(1), a)))
		})
	}, 10)
	check(len(ans) == 0, "q=/=[1,a] &&& q===[1,a]: no answer")
}
