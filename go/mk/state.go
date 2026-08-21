package mk

// Ctr is a pending disequality: "X and Y must not be unified".
type Ctr struct {
	X, Y Term
}

// CtrStore is an immutable list of pending disequalities. It is shared by
// pointer across State copies (like the C++ shared_ptr<const vector>); adding
// or dropping a constraint builds a new slice and shares the rest.
type CtrStore []Ctr

var emptyCtrs = &CtrStore{}

// State is the search state: environment (for fresh variables) + substitution
// + the store of pending disequality constraints. The substitution is
// copy-on-write and the constraint store is shared, so copying a State is O(1).
type State struct {
	env   Env
	subst Subst
	ctrs  *CtrStore
}

func EmptyState() State { return State{MakeEnv(), EmptySubst(), emptyCtrs} }

// Env exposes the run environment (its fresh counter is shared across all
// copies of the state, like OCanren's mutable Env.next).
func (s State) Env() Env { return s.env }

// Subst exposes the current substitution.
func (s State) Subst() Subst { return s.subst }

// Ctrs exposes the pending disequality store.
func (s State) Ctrs() *CtrStore { return s.ctrs }

// withSubst returns a copy of the state with a new substitution.
func (s State) withSubst(subst Subst) State {
	s.subst = subst
	return s
}

// withCtrs returns a copy of the state with a new constraint store.
func (s State) withCtrs(ctrs *CtrStore) State {
	s.ctrs = ctrs
	return s
}
