package mk

// Stream is a lazy stream, mirroring OCanren's polymorphic Stream module:
//
//	Nil | Cons of 'a * 'a stream | Thunk of unit -> 'a stream
//
// The `Waiting` constructor of OCanren is omitted: it is only used by tabling,
// which this implementation does not provide. mplus and bind follow OCanren's
// Stream.ml exactly (including eager force of thunks where the OCaml code
// forces them).

type streamKind int

const (
	stNil streamKind = iota
	stCons
	stThunk
)

type streamNode[T any] struct {
	kind streamKind
	head T                // Cons
	tail *streamNode[T]   // Cons
	zz   func() Stream[T] // Thunk
}

type Stream[T any] struct {
	node *streamNode[T]
}

func NilStream[T any]() Stream[T] { return Stream[T]{&streamNode[T]{kind: stNil}} }

func ConsStream[T any](x T, tail Stream[T]) Stream[T] {
	return Stream[T]{&streamNode[T]{kind: stCons, head: x, tail: tail.node}}
}

func FromFunc[T any](zz func() Stream[T]) Stream[T] {
	return Stream[T]{&streamNode[T]{kind: stThunk, zz: zz}}
}

func (s Stream[T]) Kind() streamKind { return s.node.kind }

// Head is valid only when Kind() == stCons.
func (s Stream[T]) Head() T { return s.node.head }

func (s Stream[T]) Tail() Stream[T] { return Stream[T]{s.node.tail} }

// Force forces a thunk eagerly, like OCanren's Stream.force.
func (s Stream[T]) Force() Stream[T] {
	if s.node.kind == stThunk {
		return s.node.zz()
	}
	return s
}

// mplus is OCanren's Stream.mplus:
//
//	mplus Nil ys         = force ys
//	mplus (Cons x xs) ys = Cons x (thunk (mplus (force ys) xs))
//	mplus (Thunk zz) ys  = thunk (mplus (force ys) (Thunk zz))
func mplus[T any](xs, ys Stream[T]) Stream[T] {
	switch xs.Kind() {
	case stNil:
		return ys.Force()
	case stCons:
		return ConsStream(xs.Head(), FromFunc(func() Stream[T] {
			return mplus(ys.Force(), xs.Tail())
		}))
	case stThunk:
		return FromFunc(func() Stream[T] { return mplus(ys.Force(), xs) })
	}
	return NilStream[T]() // unreachable
}

// streamBind is OCanren's Stream.bind: maps a stream of T to a stream of U.
func streamBind[T any, U any](s Stream[T], f func(T) Stream[U]) Stream[U] {
	switch s.Kind() {
	case stNil:
		return NilStream[U]()
	case stCons:
		return mplus(f(s.Head()), FromFunc(func() Stream[U] {
			return streamBind(s.Tail().Force(), f)
		}))
	case stThunk:
		return FromFunc(func() Stream[U] { return streamBind(s.Force(), f) })
	}
	return NilStream[U]() // unreachable
}

func msplit[T any](s Stream[T]) (T, Stream[T], bool) {
	var zero T
	switch s.Kind() {
	case stNil:
		return zero, NilStream[T](), false
	case stCons:
		return s.Head(), s.Tail(), true
	case stThunk:
		return msplit(s.Force())
	}
	return zero, NilStream[T](), false
}

// Take takes at most n elements; n < 0 means "take all".
func Take[T any](n int, s Stream[T]) []T {
	out := []T{}
	cur := s
	for n != 0 {
		x, tail, ok := msplit(cur)
		if !ok {
			break
		}
		out = append(out, x)
		cur = tail
		if n > 0 {
			n--
		}
	}
	return out
}
