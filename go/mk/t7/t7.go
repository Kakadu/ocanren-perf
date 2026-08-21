// Package t7 ports regression/test007.ml (match_lam-based substo and evalo),
// from ../cpp/src/stlc.h namespace t7.
package t7

import (
	"ocanren-go/mk"
	"ocanren-go/mk/stlc"
)

func matchLam(a mk.Term,
	onVar func(mk.Term) mk.Goal,
	onApp func(mk.Term, mk.Term) mk.Goal,
	onAbs func(mk.Term, mk.Term) mk.Goal) mk.Goal {
	// The callbacks are captured by value (a copy of the closure): the conde
	// branches are forced lazily, long after matchLam has returned.
	return mk.Conde([]mk.Goal{
		mk.Fresh(func(x mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(a, stlc.V(x)), onVar(x))
		}),
		mk.Fresh2(func(pp, qq mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(a, stlc.App(pp, qq)), onApp(pp, qq))
		}),
		mk.Fresh2(func(x, l mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(a, stlc.Abs(x, l)), onAbs(x, l))
		}),
	})
}

func Substo(l, x, a, lp mk.Term) mk.Goal {
	return matchLam(l,
		func(y mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(x, y), mk.UnifyGoal(lp, a))
		},
		func(pp, qq mk.Term) mk.Goal {
			return mk.Fresh2(func(pp2, qq2 mk.Term) mk.Goal {
				return mk.Conj(
					mk.Conj(mk.UnifyGoal(lp, stlc.App(pp2, qq2)), Substo(pp, x, a, pp2)),
					Substo(qq, x, a, qq2))
			})
		},
		func(vv, b mk.Term) mk.Goal {
			return mk.Conde([]mk.Goal{
				mk.Conj(mk.UnifyGoal(x, vv), mk.UnifyGoal(lp, l)),
				mk.Fresh(func(bp mk.Term) mk.Goal {
					return mk.Conj(mk.UnifyGoal(lp, stlc.Abs(vv, bp)), Substo(b, x, a, bp))
				}),
			})
		})
}

func Evalo(m, n mk.Term) mk.Goal {
	return matchLam(m,
		func(mk.Term) mk.Goal {
			return mk.UnifyGoal(n, m)
		},
		func(f, a mk.Term) mk.Goal {
			return mk.Fresh2(func(fp, ap mk.Term) mk.Goal {
				return mk.Conj(
					matchLam(fp,
						func(mk.Term) mk.Goal { return mk.UnifyGoal(n, stlc.App(fp, ap)) },
						func(mk.Term, mk.Term) mk.Goal { return mk.UnifyGoal(n, stlc.App(fp, ap)) },
						func(x, l mk.Term) mk.Goal {
							return mk.Fresh(func(lp mk.Term) mk.Goal {
								return mk.Conj(Substo(l, x, ap, lp), Evalo(lp, n))
							})
						}),
					mk.Conj(Evalo(f, fp), Evalo(a, ap)))
			})
		},
		func(mk.Term, mk.Term) mk.Goal {
			return mk.UnifyGoal(n, m)
		})
}
