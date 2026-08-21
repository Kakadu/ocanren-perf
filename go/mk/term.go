// Package mk is a Go port of the OCanren (miniKanren) core used by the C++
// port in ../cpp. Terms are stored Structure-of-Arrays: a Term is an int32
// index into a global arena whose fields live in parallel columns, so a term
// costs far less than a padded struct and same-kind data stays cache-local.
// Terms are never GC'd individually -- they live for the whole program and are
// shared freely between search branches (like the C++ bump arena).
package mk

// Var is a logic variable, identified by (env, index) exactly like OCanren's
// Term.Var.t: env is the anchor of the run that created it, index is unique
// within the run.
type Var struct {
	Env   int64
	Index int64
}

func (v Var) Equal(o Var) bool { return v.Index == o.Index && v.Env == o.Env }

// Compare mirrors OCanren's Term.Var.compare: index first, then env.
func (v Var) Compare(o Var) int {
	if v.Index != o.Index {
		if v.Index < o.Index {
			return -1
		}
		return 1
	}
	if v.Env != o.Env {
		if v.Env < o.Env {
			return -1
		}
		return 1
	}
	return 0
}

// NoTerm is the sentinel term index meaning "no term" (e.g. an unbound
// substitution entry).
const NoTerm Term = -1

// TermKind is the tag of a term node.
type TermKind uint8

const (
	KVar TermKind = iota
	KInt
	KStr
	KNil
	KCons
	// Simply-typed lambda calculus (GLam):
	KLamV   // V of name
	KLamApp // App of m n
	KLamAbs // Abs of x l
	// Types (GTyp):
	KTypeP   // P of string
	KTypeArr // Arr of t1 t2
	// Pairs (OCanren.Std.Pair), distinct from list cons:
	KPair // (a, b)
	// Quines / thrines (Gterm and Gresult):
	KGtSymb    // Symb of s     -- car = symbol (a Str term)
	KGtSeq     // Seq of xs     -- car = list of Gterms
	KGrVal     // Val_ of t     -- car = a Gterm
	KGrClosure // Closure s t xs-- car = symbol, cdr = Pair(body, env)
)

// Term is an index into the global SoA term arena.
type Term int32

func (t Term) Kind() TermKind { return theArena.kindOf(t) }
func (t Term) IntVal() int64  { return theArena.intv(t) }
func (t Term) VarVal() Var    { return theArena.varOf(t) }
func (t Term) Car() Term      { return theArena.carOf(t) }
func (t Term) Cdr() Term      { return theArena.cdrOf(t) }
func (t Term) StrVal() string { return theArena.strOf(t) }

// termArena is the Structure-of-Arrays storage for all terms. Column i holds
// field i of every term; a term's payload is interpreted by its kind:
//
//	Int       a64 = value
//	Var       a64 = env, b64 = index
//	Str       a64 = pool offset, b64 = length
//	Cons/...  car, cdr = child indices (NoTerm when absent)
type termArena struct {
	kind []uint8
	car  []int32
	cdr  []int32
	a64  []int64
	b64  []int64
	pool []byte // concatenated string bytes
}

var theArena = &termArena{}

func (a *termArena) make(k TermKind) Term {
	t := Term(len(a.kind))
	a.kind = append(a.kind, uint8(k))
	a.car = append(a.car, int32(NoTerm))
	a.cdr = append(a.cdr, int32(NoTerm))
	a.a64 = append(a.a64, 0)
	a.b64 = append(a.b64, 0)
	return t
}

func (a *termArena) kindOf(t Term) TermKind { return TermKind(a.kind[int(t)]) }
func (a *termArena) intv(t Term) int64      { return a.a64[int(t)] }
func (a *termArena) varOf(t Term) Var       { return Var{a.a64[int(t)], a.b64[int(t)]} }
func (a *termArena) carOf(t Term) Term      { return Term(a.car[int(t)]) }
func (a *termArena) cdrOf(t Term) Term      { return Term(a.cdr[int(t)]) }
func (a *termArena) strOf(t Term) string {
	off, ln := int(a.a64[int(t)]), int(a.b64[int(t)])
	return string(a.pool[off : off+ln])
}

// Size returns the number of terms allocated so far.
func (a *termArena) Size() int { return len(a.kind) }

// Bytes is the logical bytes used by the SoA columns plus the string pool --
// the fair comparison against the old 56-byte array-of-structs node.
func (a *termArena) Bytes() int {
	return len(a.kind)*1 + len(a.car)*4 + len(a.cdr)*4 +
		len(a.a64)*8 + len(a.b64)*8 + len(a.pool)
}

// Terms exposes the global arena (for size/bytes reporting).
func Terms() *termArena { return theArena }

// --- constructors --------------------------------------------------------

func MkVar(v Var) Term {
	t := theArena.make(KVar)
	theArena.a64[int(t)] = v.Env
	theArena.b64[int(t)] = v.Index
	return t
}

func MkInt(i int64) Term {
	t := theArena.make(KInt)
	theArena.a64[int(t)] = i
	return t
}

func MkStr(s string) Term {
	t := theArena.make(KStr)
	off := int64(len(theArena.pool))
	theArena.pool = append(theArena.pool, s...)
	theArena.a64[int(t)] = off
	theArena.b64[int(t)] = int64(len(s))
	return t
}

func MkNil() Term { return theArena.make(KNil) }

func MkCons(car, cdr Term) Term {
	t := theArena.make(KCons)
	theArena.car[int(t)] = int32(car)
	theArena.cdr[int(t)] = int32(cdr)
	return t
}

func MkLamV(name Term) Term {
	t := theArena.make(KLamV)
	theArena.car[int(t)] = int32(name)
	return t
}

func MkLamApp(m, n Term) Term {
	t := theArena.make(KLamApp)
	theArena.car[int(t)] = int32(m)
	theArena.cdr[int(t)] = int32(n)
	return t
}

func MkLamAbs(x, l Term) Term {
	t := theArena.make(KLamAbs)
	theArena.car[int(t)] = int32(x)
	theArena.cdr[int(t)] = int32(l)
	return t
}

func MkTypeP(s Term) Term {
	t := theArena.make(KTypeP)
	theArena.car[int(t)] = int32(s)
	return t
}

func MkTypeArr(t1, t2 Term) Term {
	t := theArena.make(KTypeArr)
	theArena.car[int(t)] = int32(t1)
	theArena.cdr[int(t)] = int32(t2)
	return t
}

func MkPair(a, b Term) Term {
	t := theArena.make(KPair)
	theArena.car[int(t)] = int32(a)
	theArena.cdr[int(t)] = int32(b)
	return t
}

func MkGSymb(s Term) Term {
	t := theArena.make(KGtSymb)
	theArena.car[int(t)] = int32(s) // a Str term (the symbol name)
	return t
}

func MkGSeq(xs Term) Term {
	t := theArena.make(KGtSeq)
	theArena.car[int(t)] = int32(xs) // a list of Gterms
	return t
}

func MkGVal(t Term) Term {
	r := theArena.make(KGrVal)
	theArena.car[int(r)] = int32(t) // a Gterm
	return r
}

func MkGClosure(s, body, env Term) Term {
	t := theArena.make(KGrClosure)
	theArena.car[int(t)] = int32(s) // symbol
	theArena.cdr[int(t)] = int32(MkPair(body, env))
	return t
}

// Env is the anchor of a run plus the counter for fresh variables. It mirrors
// OCanren's Env.t: `next` is a *shared mutable* counter (OCanren declares it
// `mutable` and every Env.fresh increments it in place), so fresh-variable
// indices keep increasing across all branches of a run. The first variable has
// index 10 (OCanren's `first_var`).
type Env struct {
	anchor int64
	next   *int64 // shared mutable counter
}

// Fresh returns the next variable and advances the shared counter.
func (e Env) Fresh() Var {
	v := Var{e.anchor, *e.next}
	*e.next++
	return v
}

var lastAnchor int64 = 11

// MakeEnv creates a fresh environment for a new run, following OCanren's
// anchor numbering.
func MakeEnv() Env {
	lastAnchor++
	c := int64(10)
	return Env{anchor: lastAnchor, next: &c}
}
