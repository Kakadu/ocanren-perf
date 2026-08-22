package mk

import (
	"fmt"
	"strconv"
)

// Core miniKanren machinery: walk, unify, goals, reify, run.
//
// All semantics (walk-as-you-go unification with occurs check, binding the
// smaller variable to the larger one, stream interleaving of conde, lazy defer)
// follow OCanren's Core.ml / Subst.ml / Stream.ml.

// Goal is a relation: it maps a state to a (lazy) stream of resulting states.
type Goal func(State) Stream[State]

// --- walk ----------------------------------------------------------------

// walk follows the substitution chain to the end, like OCanren's Subst.walk.
// Non-variables are returned as is; an unbound variable is returned itself.
func walk(s Subst, t Term) Term {
	for t.Kind() == KVar {
		bound := s.Lookup(t.VarVal())
		if bound == NoTerm {
			return t
		}
		t = bound
	}
	return t
}

// --- occurs check ---------------------------------------------------------

// occurs reports whether v occurs in t (variables are walked through the
// substitution on the road, like OCanren's Subst.occurs).
func occurs(s Subst, v Var, t Term) bool {
	t = walk(s, t)
	switch t.Kind() {
	case KVar:
		return t.VarVal().Equal(v)
	case KCons, KLamApp, KLamAbs, KTypeArr, KPair, KGrClosure, KGrClo3:
		return occurs(s, v, t.Car()) || occurs(s, v, t.Cdr())
	case KLamV, KTypeP, KGtSymb, KGtSeq, KGrVal, KNatSucc, KGtVR, KGtTuple, KGrCode:
		return occurs(s, v, t.Car())
	default:
		return false
	}
}

// --- disequality helpers --------------------------------------------------

type cmp int

const (
	cmpSame cmp = iota
	cmpDifferent
	cmpMaybe
)

// cmpTerms is a three-way comparison of the walked forms of two terms:
//
//	Same      -- structurally identical (variables by identity): a pending
//	             disequality is violated in this substitution.
//	Different -- a ground mismatch somewhere: the two can never unify, so the
//	             disequality is permanently satisfied and may be dropped.
//	Maybe     -- not equal yet but unifiable: keep the constraint for later.
func cmpTerms(s Subst, a, b Term) cmp {
	a = walk(s, a)
	b = walk(s, b)
	if a == b {
		return cmpSame
	}
	if a.Kind() != b.Kind() {
		// A free variable can still be unified with the other term, so the
		// disequality is NOT permanently satisfied (OCanren refines the
		// constraint to follow the variable). Only two distinct ground
		// constructors are provably different.
		if a.Kind() == KVar || b.Kind() == KVar {
			return cmpMaybe
		}
		return cmpDifferent
	}
	switch a.Kind() {
	case KVar:
		if a.VarVal().Equal(b.VarVal()) {
			return cmpSame
		}
		return cmpMaybe
	case KInt:
		if a.IntVal() == b.IntVal() {
			return cmpSame
		}
		return cmpDifferent
	case KStr:
		if a.StrVal() == b.StrVal() {
			return cmpSame
		}
		return cmpDifferent
	case KNil, KNatZero:
		return cmpSame
	case KCons, KLamApp, KLamAbs, KTypeArr, KPair, KGrClosure, KGrClo3:
		c1 := cmpTerms(s, a.Car(), b.Car())
		if c1 == cmpDifferent {
			return cmpDifferent
		}
		c2 := cmpTerms(s, a.Cdr(), b.Cdr())
		if c2 == cmpDifferent {
			return cmpDifferent
		}
		if c1 == cmpSame && c2 == cmpSame {
			return cmpSame
		}
		return cmpMaybe
	case KLamV, KTypeP, KGtSymb, KGtSeq, KGrVal, KNatSucc, KGtVR, KGtTuple, KGrCode:
		return cmpTerms(s, a.Car(), b.Car())
	}
	return cmpDifferent
}

func structurallyEqual(s Subst, a, b Term) bool { return cmpTerms(s, a, b) == cmpSame }

// recheckDiseq re-checks every pending disequality against the (new)
// substitution. ok is false when some constraint is violated; otherwise the
// reduced store (permanently-satisfied constraints dropped) is returned.
func recheckDiseq(s Subst, ctrs *CtrStore) (*CtrStore, bool) {
	out := make(CtrStore, 0, len(*ctrs))
	for _, p := range *ctrs {
		c := cmpTerms(s, p.X, p.Y)
		if c == cmpSame {
			return nil, false // violated
		}
		if c == cmpDifferent {
			continue // fulfilled, drop
		}
		out = append(out, p) // maybe, keep
	}
	return &out, true
}

// --- unify ----------------------------------------------------------------

// unify is OCanren's Subst.unify: walks both sides, binds the smaller variable
// to the larger one, performs the occurs check on every binding, and recurses
// structurally into cons cells. ok is false when unification fails.
func unify(s Subst, x, y Term) (Subst, bool) { return unifyHelper(s, x, y) }

func unifyHelper(s Subst, x, y Term) (Subst, bool) {
	xv := x.Kind() == KVar
	yv := y.Kind() == KVar
	if xv || yv {
		wx := walk(s, x)
		wy := walk(s, y)
		wxv := wx.Kind() == KVar
		wyv := wy.Kind() == KVar
		if wxv && wyv {
			vx := wx.VarVal()
			vy := wy.VarVal()
			if vx.Equal(vy) {
				return s, true
			}
			// Match OCanren Subst.unify: bind the FIRST walked var to the
			// second (extend x (repr y)), not by index order.
			if occurs(s, vx, wy) {
				return EmptySubst(), false
			}
			return s.Extend(vx, wy), true
		}
		if wxv {
			if occurs(s, wx.VarVal(), wy) {
				return EmptySubst(), false
			}
			return s.Extend(wx.VarVal(), wy), true
		}
		if wyv {
			if occurs(s, wy.VarVal(), wx) {
				return EmptySubst(), false
			}
			return s.Extend(wy.VarVal(), wx), true
		}
		return unifyHelper(s, wx, wy)
	}
	if x.Kind() != y.Kind() {
		return EmptySubst(), false
	}
	switch x.Kind() {
	case KNil, KNatZero:
		return s, true
	case KInt:
		if x.IntVal() == y.IntVal() {
			return s, true
		}
		return EmptySubst(), false
	case KStr:
		if x.StrVal() == y.StrVal() {
			return s, true
		}
		return EmptySubst(), false
	case KCons, KLamApp, KLamAbs, KTypeArr, KPair, KGrClosure, KGrClo3:
		s1, ok := unifyHelper(s, x.Car(), y.Car())
		if !ok {
			return EmptySubst(), false
		}
		return unifyHelper(s1, x.Cdr(), y.Cdr())
	case KLamV, KTypeP, KGtSymb, KGtSeq, KGrVal, KNatSucc, KGtVR, KGtTuple, KGrCode:
		return unifyHelper(s, x.Car(), y.Car())
	default:
		return EmptySubst(), false
	}
}

// --- goals ----------------------------------------------------------------

var (
	Success Goal = func(st State) Stream[State] { return ConsStream(st, NilStream[State]()) }
	Failure Goal = func(State) Stream[State] { return NilStream[State]() }
)

// --- unification counting -------------------------------------------------

// Unifications counts every executed `===` (unify) relation, success or
// failure. This mirrors OCanren's Core.stat.unification_count (gated by
// OCANREN_BENCH_COUNTS) and the TRACE Counters used in ocanren01/unif_count:
// the counter is bumped once per `===` application to a state, before the
// unification is attempted. The search is single-threaded, so a plain int64
// suffices (no atomics).
var Unifications int64

// ResetUnifications zeroes the counter (call before each measured benchmark).
func ResetUnifications() { Unifications = 0 }

// UnificationCount returns the number of `===` executions so far.
func UnificationCount() int64 { return Unifications }

// MaxUnifications, when > 0, caps the search: once the counter reaches it, every
// further `===` fails, which turns an otherwise non-productive (infinite) search
// into a finite one so we can observe how far the search gets before giving up.
// 0 means unlimited. Debug aid only; leave 0 for real benchmarks.
var MaxUnifications int64

// UnifyGoal is OCanren's `===`: unify, then re-check the pending disequalities
// against the new substitution (a unification may violate a constraint). Fails
// the branch when some disequality is violated.
func UnifyGoal(x, y Term) Goal {
	return func(st State) Stream[State] {
		Unifications++ // count every === execution, even ones that fail below
		if MaxUnifications > 0 && Unifications > MaxUnifications {
			return NilStream[State]() // step limit reached: fail this branch
		}
		res, ok := unify(st.subst, x, y)
		if !ok {
			return NilStream[State]()
		}
		st2 := st.withSubst(res)
		if len(*st.ctrs) != 0 {
			ctrs, ok2 := recheckDiseq(res, st.ctrs)
			if !ok2 {
				return NilStream[State]() // disequality violated
			}
			st2 = st2.withCtrs(ctrs)
		}
		return ConsStream(st2, NilStream[State]())
	}
}

// DiseqGoal is OCanren's `=/=`: assert that x and y are not unified. Fails
// immediately when they are already equal; otherwise records the constraint.
func DiseqGoal(x, y Term) Goal {
	return func(st State) Stream[State] {
		if structurallyEqual(st.subst, x, y) {
			return NilStream[State]()
		}
		out := make(CtrStore, len(*st.ctrs), len(*st.ctrs)+1)
		copy(out, *st.ctrs)
		out = append(out, Ctr{x, y})
		st2 := st.withCtrs(&out)
		return ConsStream(st2, NilStream[State]())
	}
}

func Conj(f, g Goal) Goal {
	return func(st State) Stream[State] { return streamBind(f(st), g) }
}

// Disj is OCanren's disj: branches combined with mplus, the right one delayed.
func Disj(f, g Goal) Goal {
	return func(st State) Stream[State] {
		return mplus(f(st), FromFunc(func() Stream[State] { return g(st) }))
	}
}

// Conde is OCanren's conde: mplus over the branches, everything delayed by one
// thunk, so the whole conde is lazy even when the first branch fails.
func Conde(gs []Goal) Goal {
	return func(st State) Stream[State] {
		var inner func(i int) Stream[State]
		inner = func(i int) Stream[State] {
			if i+1 == len(gs) {
				return gs[i](st)
			}
			return mplus(gs[i](st), FromFunc(func() Stream[State] { return inner(i + 1) }))
		}
		return FromFunc(func() Stream[State] { return inner(0) })
	}
}

// Fresh is OCanren's `fresh (a) body`. The ppx_fresh desugaring is
// `Fresh.one (fun a -> delay (fun () -> body))`: the variable is created
// eagerly, then the body is wrapped in a single delay. The delay keeps recursive
// relations productive, so it must be reproduced exactly.
func Fresh(f func(Term) Goal) Goal {
	return func(st State) Stream[State] {
		x := MkVar(st.env.Fresh())
		return Defer(func() Goal { return f(x) })(st)
	}
}

func Fresh2(f func(Term, Term) Goal) Goal {
	return func(st State) Stream[State] {
		x := MkVar(st.env.Fresh())
		y := MkVar(st.env.Fresh())
		return Defer(func() Goal { return f(x, y) })(st)
	}
}

func Fresh3(f func(Term, Term, Term) Goal) Goal {
	return func(st State) Stream[State] {
		x := MkVar(st.env.Fresh())
		y := MkVar(st.env.Fresh())
		z := MkVar(st.env.Fresh())
		return Defer(func() Goal { return f(x, y, z) })(st)
	}
}

func Fresh4(f func(Term, Term, Term, Term) Goal) Goal {
	return func(st State) Stream[State] {
		x := MkVar(st.env.Fresh())
		y := MkVar(st.env.Fresh())
		z := MkVar(st.env.Fresh())
		w := MkVar(st.env.Fresh())
		return Defer(func() Goal { return f(x, y, z, w) })(st)
	}
}

func Fresh5(f func(Term, Term, Term, Term, Term) Goal) Goal {
	return func(st State) Stream[State] {
		x := MkVar(st.env.Fresh())
		y := MkVar(st.env.Fresh())
		z := MkVar(st.env.Fresh())
		w := MkVar(st.env.Fresh())
		u := MkVar(st.env.Fresh())
		return Defer(func() Goal { return f(x, y, z, w, u) })(st)
	}
}

func Fresh6(f func(Term, Term, Term, Term, Term, Term) Goal) Goal {
	return func(st State) Stream[State] {
		a := MkVar(st.env.Fresh())
		b := MkVar(st.env.Fresh())
		c := MkVar(st.env.Fresh())
		d := MkVar(st.env.Fresh())
		e := MkVar(st.env.Fresh())
		ff := MkVar(st.env.Fresh())
		return Defer(func() Goal { return f(a, b, c, d, e, ff) })(st)
	}
}

func Fresh7(f func(Term, Term, Term, Term, Term, Term, Term) Goal) Goal {
	return func(st State) Stream[State] {
		a := MkVar(st.env.Fresh())
		b := MkVar(st.env.Fresh())
		c := MkVar(st.env.Fresh())
		d := MkVar(st.env.Fresh())
		e := MkVar(st.env.Fresh())
		ff := MkVar(st.env.Fresh())
		g := MkVar(st.env.Fresh())
		return Defer(func() Goal { return f(a, b, c, d, e, ff, g) })(st)
	}
}

func Fresh8(f func(Term, Term, Term, Term, Term, Term, Term, Term) Goal) Goal {
	return func(st State) Stream[State] {
		a := MkVar(st.env.Fresh())
		b := MkVar(st.env.Fresh())
		c := MkVar(st.env.Fresh())
		d := MkVar(st.env.Fresh())
		e := MkVar(st.env.Fresh())
		ff := MkVar(st.env.Fresh())
		g := MkVar(st.env.Fresh())
		h := MkVar(st.env.Fresh())
		return Defer(func() Goal { return f(a, b, c, d, e, ff, g, h) })(st)
	}
}

// Defer is OCanren's delay/defer: the goal body is not even constructed until
// the stream is forced, which keeps recursive relations like reverso productive.
func Defer(mkGoal func() Goal) Goal {
	return func(st State) Stream[State] {
		return FromFunc(func() Stream[State] { return mkGoal()(st) })
	}
}

// --- reify and show -------------------------------------------------------

// Reify replaces bound variables by their (walked) values; unbound ones stay as
// variables and print as _.N. Mirrors OCanren's Subst.reify / Logic.Reifier.reify.
func Reify(s Subst, t Term) Term {
	switch t.Kind() {
	case KVar:
		w := walk(s, t)
		if w.Kind() == KVar {
			return w
		}
		return Reify(s, w)
	case KCons:
		return MkCons(Reify(s, t.Car()), Reify(s, t.Cdr()))
	case KLamV:
		return MkLamV(Reify(s, t.Car()))
	case KLamApp:
		return MkLamApp(Reify(s, t.Car()), Reify(s, t.Cdr()))
	case KLamAbs:
		return MkLamAbs(Reify(s, t.Car()), Reify(s, t.Cdr()))
	case KTypeP:
		return MkTypeP(Reify(s, t.Car()))
	case KTypeArr:
		return MkTypeArr(Reify(s, t.Car()), Reify(s, t.Cdr()))
	case KPair:
		return MkPair(Reify(s, t.Car()), Reify(s, t.Cdr()))
	case KGtSymb:
		return MkGSymb(Reify(s, t.Car()))
	case KGtSeq:
		return MkGSeq(Reify(s, t.Car()))
	case KGrVal:
		return MkGVal(Reify(s, t.Car()))
	case KGrClosure:
		return MkGClosure(Reify(s, t.Car()), Reify(s, t.Cdr()).Car(), Reify(s, t.Cdr()).Cdr())
	case KNatSucc:
		return MkNatSucc(Reify(s, t.Car()))
	case KGtVR:
		return MkGtVR(Reify(s, t.Car()))
	case KGtTuple:
		return MkGtTuple(Reify(s, t.Car()))
	case KGrClo3:
		return MkGrClo3(Reify(s, t.Car()), Reify(s, t.Cdr()).Car(), Reify(s, t.Cdr()).Cdr())
	case KGrCode:
		return MkGtCode(Reify(s, t.Car()))
	default:
		return t // Int, Str, Nil, KNatZero
	}
}

// Show pretty-prints a reified term in OCanren's style.
func Show(t Term) string {
	switch t.Kind() {
	case KVar:
		return fmt.Sprintf("_.%d", t.VarVal().Index)
	case KInt:
		return strconv.FormatInt(t.IntVal(), 10)
	case KStr:
		return "\"" + t.StrVal() + "\""
	case KNil:
		return "[]"
	case KLamV:
		return "V (" + Show(t.Car()) + ")"
	case KLamApp:
		return "App (" + Show(t.Car()) + ", " + Show(t.Cdr()) + ")"
	case KLamAbs:
		return "Abs (" + Show(t.Car()) + ", " + Show(t.Cdr()) + ")"
	case KTypeP:
		return "P (" + Show(t.Car()) + ")"
	case KTypeArr:
		return "Arr (" + Show(t.Car()) + ", " + Show(t.Cdr()) + ")"
	case KPair:
		return "(" + Show(t.Car()) + ", " + Show(t.Cdr()) + ")"
	case KGtSymb:
		// The symbol name is usually a ground string, but can be a free
		// variable (or other term) in a partially-reified answer.
		if t.Car().Kind() == KStr {
			return "(symb '" + t.Car().StrVal() + ")"
		}
		return "(symb " + Show(t.Car()) + ")"
	case KGtSeq:
		return "(seq " + Show(t.Car()) + ")"
	case KGrVal:
		return "val (" + Show(t.Car()) + ")"
	case KGrClosure:
		return "closure (" + Show(t.Car()) + ", " + Show(t.Cdr()) + ")"
	case KNatZero:
		return "z"
	case KNatSucc:
		return "(s " + Show(t.Car()) + ")"
	case KGtVR:
		return "(vr " + Show(t.Car()) + ")"
	case KGtTuple:
		return "(" + Show(t.Car()) + ")"
	case KGrClo3:
		return "clo (" + Show(t.Car()) + ", " + Show(t.Cdr()) + ")"
	case KGrCode:
		return "code (" + Show(t.Car()) + ")"
	case KCons:
		out := "["
		cur := t
		first := true
		for cur.Kind() == KCons {
			if !first {
				out += ", "
			}
			out += Show(cur.Car())
			cur = cur.Cdr()
			first = false
		}
		if cur.Kind() == KNil {
			out += "]"
		} else {
			out += " | " + Show(cur) + "]"
		}
		return out
	}
	return "?"
}

// --- run ------------------------------------------------------------------

// Run runs a query: `query` maps the (fresh) query variable to a goal. It
// returns a (lazy) stream of reified terms, one per answer state.
func Run(query func(Term) Goal) Stream[Term] {
	return FromFunc(func() Stream[Term] {
		st := EmptyState()
		q := MkVar(st.env.Fresh())
		g := query(q)
		reifyStep := func(s State) Stream[Term] {
			return ConsStream(Reify(s.subst, q), NilStream[Term]())
		}
		return streamBind(g(st), reifyStep)
	})
}
