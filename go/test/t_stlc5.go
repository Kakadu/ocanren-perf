package main

import (
	"ocanren-go/mk"
	stlc "ocanren-go/mk/stlc"
	t5 "ocanren-go/mk/t5"
)

// env1 / env2 build type environments: a logic list of (name, type) pairs.
func env1(a, b mk.Term) mk.Term { return mk.MkCons(mk.MkPair(a, b), mk.MkNil()) }

func env2(a, b, c, d mk.Term) mk.Term {
	return mk.MkCons(mk.MkPair(a, b), mk.MkCons(mk.MkPair(c, d), mk.MkNil()))
}

// --- regression/test005.ml ---------------------------------------------

func test005LookupoEmpty() {
	ans := answers(func(q mk.Term) mk.Goal { return t5.Lookupo(stlc.VarX(), mk.MkNil(), q) }, 1)
	check(len(ans) == 0, "lookupo varX [] q: no answer")
}

func test005LookupoSingle() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t5.Lookupo(stlc.VarX(), env1(stlc.VarX(), stlc.V(stlc.VarX())), q)
	}, 1)
	check(len(ans) == 1, "lookupo varX [(x,V x)] q: 1 answer")
	checkEq(mk.Show(ans[0]), `V ("x")`, "lookupo varX [(x,V x)] q")
}

func test005LookupoTwo() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t5.Lookupo(stlc.VarX(), env2(stlc.VarY(), stlc.V(stlc.VarY()), stlc.VarX(), stlc.V(stlc.VarX())), q)
	}, 1)
	check(len(ans) == 1, "lookupo varX [(y,V y),(x,V x)] q: 1 answer")
	checkEq(mk.Show(ans[0]), `V ("x")`, "lookupo varX [(y,V y),(x,V x)] q")
}

func test005LookupoFindX() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t5.Lookupo(q, env2(stlc.VarY(), stlc.V(stlc.VarY()), stlc.VarX(), stlc.V(stlc.VarX())), stlc.V(stlc.VarX()))
	}, 1)
	check(len(ans) == 1, "lookupo q env (V x): 1 answer")
	checkEq(mk.Show(ans[0]), `"x"`, "lookupo q env (V x) = x")
}

func test005LookupoFindY() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t5.Lookupo(q, env2(stlc.VarY(), stlc.V(stlc.VarY()), stlc.VarX(), stlc.V(stlc.VarX())), stlc.V(stlc.VarY()))
	}, 1)
	check(len(ans) == 1, "lookupo q env (V y): 1 answer")
	checkEq(mk.Show(ans[0]), `"y"`, "lookupo q env (V y) = y")
}

func test005InferoUntypable() {
	// abs x (app (v x) (v x)) is not typable in STLC.
	ans := answers(func(q mk.Term) mk.Goal {
		return t5.Infero(stlc.Abs(stlc.VarX(), stlc.App(stlc.V(stlc.VarX()), stlc.V(stlc.VarX()))), q)
	}, 1)
	check(len(ans) == 0, "infero (abs x (x x)) q: no answer")
}

func test005LookupoFree() {
	// lookupo varX q (V y) -- q = [("x", V ("y")) | _.13].
	ans := answers(func(q mk.Term) mk.Goal { return t5.Lookupo(stlc.VarX(), q, stlc.V(stlc.VarY())) }, 1)
	check(len(ans) == 1, "lookupo varX q (V y): 1 answer")
	checkEq(mk.Show(ans[0]), "[(\"x\", V (\"y\")) | _.13]", "lookupo varX q (V y)")
}

func test005InferoId() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t5.Infero(stlc.Abs(stlc.VarX(), stlc.V(stlc.VarX())), q)
	}, 1)
	check(len(ans) == 1, "infero (abs x (V x)) q: 1 answer")
	checkEq(mk.Show(ans[0]), "Arr (_.21, _.21)", "infero (abs x (V x)) q")
}

func test005InferoCompose() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t5.Infero(stlc.Abs(stlc.VarF(), stlc.Abs(stlc.VarX(),
			stlc.App(stlc.V(stlc.VarF()), stlc.V(stlc.VarX())))), q)
	}, 1)
	check(len(ans) == 1, "infero (abs f (abs x (f x))) q: 1 answer")
	checkEq(mk.Show(ans[0]), "Arr (Arr (_.54, _.26), Arr (_.54, _.26))", "infero (abs f (abs x (f x))) q")
}

func test005InferoFlip() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t5.Infero(stlc.Abs(stlc.VarX(), stlc.Abs(stlc.VarF(),
			stlc.App(stlc.V(stlc.VarF()), stlc.V(stlc.VarX())))), q)
	}, 1)
	check(len(ans) == 1, "infero (abs x (abs f (f x))) q: 1 answer")
	checkEq(mk.Show(ans[0]), "Arr (_.64, Arr (Arr (_.64, _.26), _.26))", "infero (abs x (abs f (f x))) q")
}

func test005InferoFromType() {
	ans := answers(func(q mk.Term) mk.Goal {
		return t5.Infero(q, stlc.Arr(stlc.P(stlc.VarX()), stlc.P(stlc.VarX())))
	}, 1)
	check(len(ans) == 1, "infero q (Arr (P x, P x)): 1 answer")
	checkEq(mk.Show(ans[0]), "Abs (_.30, V (_.30))", "infero q (Arr (P x, P x))")
}
