// Package t6 ports regression/test006.ml (substo, evalo and a_la_quine --
// quines in lambda calculus), from ../cpp/src/stlc.h namespace t6.
package t6

import (
	"ocanren-go/mk"
	"ocanren-go/mk/stlc"
)

func Substo(l, x, a, lp mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Fresh(func(y mk.Term) mk.Goal {
			return mk.Conj(mk.Conj(mk.UnifyGoal(l, stlc.V(y)), mk.UnifyGoal(y, x)), mk.UnifyGoal(lp, a))
		}),
		mk.Fresh4(func(m, n, mp, np mk.Term) mk.Goal {
			return mk.Conj(
				mk.Conj(mk.Conj(mk.UnifyGoal(l, stlc.App(m, n)), mk.UnifyGoal(lp, stlc.App(mp, np))), Substo(m, x, a, mp)),
				Substo(n, x, a, np))
		}),
		mk.Fresh2(func(vv, b mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(l, stlc.Abs(vv, b)),
				mk.Conde([]mk.Goal{
					mk.Conj(mk.UnifyGoal(x, vv), mk.UnifyGoal(lp, l)),
					mk.Fresh(func(bp mk.Term) mk.Goal {
						return mk.Conj(mk.UnifyGoal(lp, stlc.Abs(vv, bp)), Substo(b, x, a, bp))
					}),
				}))
		}),
	})
}

func Evalo(m, n mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Fresh(func(x mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(m, stlc.V(x)), mk.UnifyGoal(n, m))
		}),
		mk.Fresh2(func(x, l mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(m, stlc.Abs(x, l)), mk.UnifyGoal(n, m))
		}),
		mk.Fresh4(func(f, a, fp, ap mk.Term) mk.Goal {
			return mk.Conj(
				mk.Conj(mk.Conj(mk.UnifyGoal(m, stlc.App(f, a)), Evalo(f, fp)), Evalo(a, ap)),
				mk.Conde([]mk.Goal{
					mk.Fresh3(func(x, l, lp mk.Term) mk.Goal {
						return mk.Conj(mk.Conj(mk.UnifyGoal(fp, stlc.Abs(x, l)), Substo(l, x, ap, lp)), Evalo(lp, n))
					}),
					mk.Fresh2(func(pp, qq mk.Term) mk.Goal {
						return mk.Conj(mk.UnifyGoal(fp, stlc.App(pp, qq)), mk.UnifyGoal(n, stlc.App(fp, ap)))
					}),
					mk.Fresh(func(x mk.Term) mk.Goal {
						return mk.Conj(mk.UnifyGoal(fp, stlc.V(x)), mk.UnifyGoal(n, stlc.App(fp, ap)))
					}),
				}))
		}),
	})
}

// ALaQuine is a_la_quine q r s = evalo (app q r) s &&& evalo (app r s) q &&&
// evalo (app s q) r.
func ALaQuine(q, r, s mk.Term) mk.Goal {
	return mk.Conj(
		mk.Conj(Evalo(stlc.App(q, r), s), Evalo(stlc.App(r, s), q)),
		Evalo(stlc.App(s, q), r))
}
