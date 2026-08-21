// Package t5 ports regression/test005.ml (lookupo and infero for simply-typed
// lambda calculus types), from ../cpp/src/stlc.h namespace t5.
package t5

import (
	"ocanren-go/mk"
	"ocanren-go/mk/stlc"
)

func Lookupo(a, g, t mk.Term) mk.Goal {
	return mk.Fresh3(func(ap, tp, tl mk.Term) mk.Goal {
		return mk.Conj(
			mk.UnifyGoal(g, mk.MkCons(stlc.Pair(ap, tp), tl)),
			mk.Conde([]mk.Goal{
				mk.Conj(mk.UnifyGoal(ap, a), mk.UnifyGoal(tp, t)),
				Lookupo(a, tl, t),
			}))
	})
}

// infero is the 3-argument relation (gamma, expr, typ).
func infero(gamma, expr, typ mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Fresh(func(x mk.Term) mk.Goal {
			return mk.Conj(mk.UnifyGoal(expr, stlc.V(x)), Lookupo(x, gamma, typ))
		}),
		mk.Fresh3(func(m, n, t mk.Term) mk.Goal {
			return mk.Conj(
				mk.Conj(mk.UnifyGoal(expr, stlc.App(m, n)), infero(gamma, m, stlc.Arr(t, typ))),
				infero(gamma, n, t))
		}),
		mk.Fresh4(func(x, l, t, tp mk.Term) mk.Goal {
			return mk.Conj(
				mk.Conj(mk.UnifyGoal(expr, stlc.Abs(x, l)), mk.UnifyGoal(typ, stlc.Arr(t, tp))),
				infero(mk.MkCons(stlc.Pair(x, t), gamma), l, tp))
		}),
	})
}

// Infero is infero expr typ = infero nil expr typ.
func Infero(expr, typ mk.Term) mk.Goal { return infero(mk.MkNil(), expr, typ) }
