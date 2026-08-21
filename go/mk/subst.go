package mk

// Subst is the substitution: a copy-on-write map from variables to terms. It
// is a persistent (functional) AVL tree -- the same data structure OCanren
// uses (Term.VarMap = Map.Make(Var)). Extend copies only the search path
// (O(log n) fresh nodes) and shares everything else, so every old version of
// the substitution stays valid. That lets a branching search keep per-branch
// substitutions for free: a State is just a pointer to a tree root.
type Subst struct {
	root *substNode
}

type substNode struct {
	key    Var
	val    Term
	left   *substNode
	right  *substNode
	height uint8
}

func EmptySubst() Subst { return Subst{nil} }

func (s Subst) IsEmpty() bool { return s.root == nil }

// Lookup returns the bound term, or NoTerm when the variable is unbound.
func (s Subst) Lookup(key Var) Term {
	n := s.root
	for n != nil {
		c := key.Compare(n.key)
		if c == 0 {
			return n.val
		}
		if c < 0 {
			n = n.left
		} else {
			n = n.right
		}
	}
	return NoTerm
}

// Extend returns a new substitution with key -> val added (replacing an
// existing binding, as OCaml's Map.add does).
func (s Subst) Extend(key Var, val Term) Subst { return Subst{insert(s.root, key, val)} }

func (s Subst) Size() int { return substSize(s.root) }

func nodeHeight(n *substNode) uint8 {
	if n == nil {
		return 0
	}
	return n.height
}

func newSubstNode(key Var, val Term, l, r *substNode) *substNode {
	h := uint8(1)
	if lh, rh := nodeHeight(l), nodeHeight(r); lh > rh {
		h += lh
	} else {
		h += rh
	}
	return &substNode{key: key, val: val, left: l, right: r, height: h}
}

func balance(n *substNode) int {
	return int(nodeHeight(n.left)) - int(nodeHeight(n.right))
}

// Persistent rotations: rebuild only the nodes on the rotation path.
func rotateLeft(n *substNode) *substNode {
	r := n.right
	return newSubstNode(r.key, r.val, newSubstNode(n.key, n.val, n.left, r.left), r.right)
}

func rotateRight(n *substNode) *substNode {
	l := n.left
	return newSubstNode(l.key, l.val, l.left, newSubstNode(n.key, n.val, l.right, n.right))
}

func rebalance(n *substNode) *substNode {
	b := balance(n)
	if b > 1 {
		if balance(n.left) < 0 {
			n = newSubstNode(n.key, n.val, rotateLeft(n.left), n.right)
		}
		return rotateRight(n)
	}
	if b < -1 {
		if balance(n.right) > 0 {
			n = newSubstNode(n.key, n.val, n.left, rotateRight(n.right))
		}
		return rotateLeft(n)
	}
	return n
}

func insert(n *substNode, key Var, val Term) *substNode {
	if n == nil {
		return newSubstNode(key, val, nil, nil)
	}
	c := key.Compare(n.key)
	if c == 0 {
		return newSubstNode(key, val, n.left, n.right)
	}
	if c < 0 {
		return rebalance(newSubstNode(n.key, n.val, insert(n.left, key, val), n.right))
	}
	return rebalance(newSubstNode(n.key, n.val, n.left, insert(n.right, key, val)))
}

func substSize(n *substNode) int {
	if n == nil {
		return 0
	}
	return 1 + substSize(n.left) + substSize(n.right)
}
