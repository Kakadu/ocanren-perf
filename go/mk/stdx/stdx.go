// Package stdx ports OCanren's Std.List relations (appendo, reverso) from
// ../cpp/src/std.h.
package stdx

import "ocanren-go/mk"

func Nil() mk.Term { return mk.MkNil() }

func Cons(h, t mk.Term) mk.Term { return mk.MkCons(h, t) }

// List builds a logic list from integers: List(1, 2) == [1, 2].
func List(xs ...int64) mk.Term {
	acc := Nil()
	for i := len(xs) - 1; i >= 0; i-- {
		acc = Cons(mk.MkInt(xs[i]), acc)
	}
	return acc
}

// Appendo is OCanren's appendo a b ab.
func Appendo(a, b, ab mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(a, Nil()), mk.UnifyGoal(b, ab)),
		mk.Fresh3(func(h, t, ab1 mk.Term) mk.Goal {
			return mk.Defer(func() mk.Goal {
				return mk.Conj(
					mk.Conj(mk.UnifyGoal(a, Cons(h, t)), mk.UnifyGoal(Cons(h, ab1), ab)),
					Appendo(t, b, ab1))
			})
		}),
	})
}

// Reverso is OCanren's reverso a b, ported from the `fresh`-macro form
// (Fresh.three + one delay around the whole conjunction, like Appendo above).
// An extra Defer around just the recursive call adds 2 unifications on
// non-trivial inputs and breaks the count agreement with OCanren.
func Reverso(a, b mk.Term) mk.Goal {
	return mk.Conde([]mk.Goal{
		mk.Conj(mk.UnifyGoal(a, Nil()), mk.UnifyGoal(b, Nil())),
		mk.Fresh3(func(h, t, a1 mk.Term) mk.Goal {
			return mk.Defer(func() mk.Goal {
				return mk.Conj(
					mk.Conj(mk.UnifyGoal(a, Cons(h, t)), Appendo(a1, Cons(h, Nil()), b)),
					Reverso(t, a1))
			})
		}),
	})
}
