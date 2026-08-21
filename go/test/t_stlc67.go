package main

import (
	"ocanren-go/mk"
	stlc "ocanren-go/mk/stlc"
	t6 "ocanren-go/mk/t6"
	t7 "ocanren-go/mk/t7"
)

// --- regression/test006.ml ---------------------------------------------

func test006Substo() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t6.Substo(stlc.V(stlc.VarX()), stlc.VarX(), stlc.V(stlc.VarY()), q)
	}, 1)
	check(len(ans) == 1, "substo (V x) x (V y) q: 1 answer")
	checkEq(mk.Show(ans[0]), `V ("y")`, "substo (V x) x (V y) q")
}

func test006EvaloAbs() {
	ans := answers(func(q mk.Term) mk.Goal { return t6.Evalo(stlc.Abs(stlc.VarX(), stlc.V(stlc.VarX())), q) }, 1)
	check(len(ans) == 1, "evalo (abs x (V x)) q: 1 answer")
	checkEq(mk.Show(ans[0]), `Abs ("x", V ("x"))`, "evalo (abs x (V x)) q")
}

func test006EvaloApp() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t6.Evalo(stlc.App(stlc.Abs(stlc.VarX(), stlc.V(stlc.VarX())), stlc.V(stlc.VarY())), q)
	}, 1)
	check(len(ans) == 1, "evalo ((abs x (V x)) (V y)) q: 1 answer")
	checkEq(mk.Show(ans[0]), `V ("y")`, "evalo ((abs x (V x)) (V y)) q")
}

func test006EvaloAppFreeArg() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t6.Evalo(stlc.App(stlc.Abs(stlc.VarX(), stlc.V(stlc.VarX())), q), stlc.V(stlc.VarY()))
	}, 1)
	check(len(ans) == 1, "evalo ((abs x (V x)) q) (V y): 1 answer")
	checkEq(mk.Show(ans[0]), `V ("y")`, "evalo ((abs x (V x)) q) (V y)")
}

func test006EvaloAppFreeBody() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t6.Evalo(stlc.App(stlc.Abs(stlc.VarX(), q), stlc.V(stlc.VarY())), stlc.V(stlc.VarY()))
	}, 1)
	check(len(ans) == 1, "evalo ((abs x q) (V y)) (V y): 1 answer")
	checkEq(mk.Show(ans[0]), `V ("x")`, "evalo ((abs x q) (V y)) (V y)")
}

func test006EvaloAppVar() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t6.Evalo(stlc.App(stlc.V(stlc.VarX()), stlc.V(stlc.VarX())), q)
	}, 1)
	check(len(ans) == 1, "evalo ((V x) (V x)) q: 1 answer")
	checkEq(mk.Show(ans[0]), `App (V ("x"), V ("x"))`, "evalo ((V x) (V x)) q")
}

func test006EvaloVar() {
	ans := answers(func(q mk.Term) mk.Goal { return t6.Evalo(stlc.V(stlc.VarX()), q) }, 1)
	check(len(ans) == 1, "evalo (V x) q: 1 answer")
	checkEq(mk.Show(ans[0]), `V ("x")`, "evalo (V x) q")
}

func test006EvaloFreeFun() {
	// evalo (app q (v varX)) (v varX) -- q = Abs (_.59, V (_.59)).
	ans := answers(func(q mk.Term) mk.Goal { return t6.Evalo(stlc.App(q, stlc.V(stlc.VarX())), stlc.V(stlc.VarX())) }, 1)
	check(len(ans) == 1, "evalo (app q (V x)) (V x): 1 answer")
	checkEqNorm(mk.Show(ans[0]), "Abs (_.59, V (_.59))", "evalo (app q (V x)) (V x)")
}

func test006EvaloFreeFunArg() {
	// evalo (app r q) (v varX) -- q=V ("x"); r=Abs (_.68, V (_.68)).
	ans := answers2(func(q, r mk.Term) mk.Goal { return t6.Evalo(stlc.App(r, q), stlc.V(stlc.VarX())) }, 1)
	check(len(ans) == 1, "evalo (app r q) (V x): 1 answer")
	checkEqNorm(mk.Show(ans[0][0])+"; "+mk.Show(ans[0][1]), `V ("x"); Abs (_.68, V (_.68))`, "evalo (app r q) (V x)")
}

func test006ALaQuine() {
	ans := answers3(func(q, r, s mk.Term) mk.Goal { return t6.ALaQuine(q, r, s) }, 2)
	check(len(ans) == 2, "a_la_quine q r s: 2 answers")
	if len(ans) == 2 {
		checkEqNorm(mk.Show(ans[0][0])+"; "+mk.Show(ans[0][1])+"; "+mk.Show(ans[0][2]),
			"Abs (_.668, V (_.668)); Abs (_.668, V (_.668)); Abs (_.668, V (_.668))",
			"a_la_quine answer 1")
		checkEqNorm(mk.Show(ans[1][0])+"; "+mk.Show(ans[1][1])+"; "+mk.Show(ans[1][2]),
			"Abs (_.783, V (_.783)); Abs (_.783, Abs (_.783, V (_.783))); Abs (_.783, Abs (_.783, V (_.783)))",
			"a_la_quine answer 2")
	}
}

// --- regression/test007.ml ---------------------------------------------

func test007Substo() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t7.Substo(stlc.V(stlc.VarX()), stlc.VarX(), stlc.V(stlc.VarY()), q)
	}, 1)
	check(len(ans) == 1, "t7 substo (V x) x (V y) q: 1 answer")
	checkEq(mk.Show(ans[0]), `V ("y")`, "t7 substo (V x) x (V y) q")
}

func test007EvaloAbs() {
	ans := answers(func(q mk.Term) mk.Goal { return t7.Evalo(stlc.Abs(stlc.VarX(), stlc.V(stlc.VarX())), q) }, 1)
	check(len(ans) == 1, "t7 evalo (abs x (V x)) q: 1 answer")
	checkEq(mk.Show(ans[0]), `Abs ("x", V ("x"))`, "t7 evalo (abs x (V x)) q")
}

func test007EvaloApp() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t7.Evalo(stlc.App(stlc.Abs(stlc.VarX(), stlc.V(stlc.VarX())), stlc.V(stlc.VarY())), q)
	}, 1)
	check(len(ans) == 1, "t7 evalo ((abs x (V x)) (V y)) q: 1 answer")
	checkEq(mk.Show(ans[0]), `V ("y")`, "t7 evalo ((abs x (V x)) (V y)) q")
}

func test007EvaloAppFreeArg() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t7.Evalo(stlc.App(stlc.Abs(stlc.VarX(), stlc.V(stlc.VarX())), q), stlc.V(stlc.VarY()))
	}, 1)
	check(len(ans) == 1, "t7 evalo ((abs x (V x)) q) (V y): 1 answer")
	checkEq(mk.Show(ans[0]), `V ("y")`, "t7 evalo ((abs x (V x)) q) (V y)")
}

func test007EvaloAppFreeBody() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t7.Evalo(stlc.App(stlc.Abs(stlc.VarX(), q), stlc.V(stlc.VarY())), stlc.V(stlc.VarY()))
	}, 1)
	check(len(ans) == 1, "t7 evalo ((abs x q) (V y)) (V y): 1 answer")
	checkEq(mk.Show(ans[0]), `V ("x")`, "t7 evalo ((abs x q) (V y)) (V y)")
}

func test007EvaloAppVar() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t7.Evalo(stlc.App(stlc.V(stlc.VarX()), stlc.V(stlc.VarX())), q)
	}, 1)
	check(len(ans) == 1, "t7 evalo ((V x) (V x)) q: 1 answer")
	checkEq(mk.Show(ans[0]), `App (V ("x"), V ("x"))`, "t7 evalo ((V x) (V x)) q")
}

func test007EvaloVar() {
	ans := answers(func(q mk.Term) mk.Goal { return t7.Evalo(stlc.V(stlc.VarX()), q) }, 1)
	check(len(ans) == 1, "t7 evalo (V x) q: 1 answer")
	checkEq(mk.Show(ans[0]), `V ("x")`, "t7 evalo (V x) q")
}

func test007EvaloFreeFun() {
	// evalo (app q (v varX)) (v varX) -- q = Abs (_.39, V (_.39)).
	ans := answers(func(q mk.Term) mk.Goal { return t7.Evalo(stlc.App(q, stlc.V(stlc.VarX())), stlc.V(stlc.VarX())) }, 1)
	check(len(ans) == 1, "t7 evalo (app q (V x)) (V x): 1 answer")
	checkEqNorm(mk.Show(ans[0]), "Abs (_.39, V (_.39))", "t7 evalo (app q (V x)) (V x)")
}

func test007EvaloFreeFunArg() {
	// evalo (app r q) (v varX) -- q=V ("x"); r=Abs (_.40, V (_.40)).
	ans := answers2(func(q, r mk.Term) mk.Goal { return t7.Evalo(stlc.App(r, q), stlc.V(stlc.VarX())) }, 1)
	check(len(ans) == 1, "t7 evalo (app r q) (V x): 1 answer")
	checkEqNorm(mk.Show(ans[0][0])+"; "+mk.Show(ans[0][1]), `V ("x"); Abs (_.40, V (_.40))`, "t7 evalo (app r q) (V x)")
}
