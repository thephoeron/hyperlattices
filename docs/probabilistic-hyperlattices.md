# Probabilistic Hyperlattices

## About

A probabilistic hyperlattice is a hyperlattice in which each element is associated with a probability. We can represent a probabilistic hyperlattice in Common Lisp using a hash table to store the elements and their probabilities, and a function to compute the supremum and infimum.

## Usage

This library defines a class called `prob-hyperlattice` that represents a probabilistic hyperlattice with a hash table to store the elements and their probabilities, and functions to compute the supremum and infimum. It also defines functions to add, remove, and check if an element is in the probabilistic hyperlattice, as well as functions to compute the supremum, infimum, and closure of a set of probabilistic lattices.

You can create a new probabilistic hyperlattice by creating an instance of the `prob-hyperlattice` class and adding elements with their probabilities to it using the `prob-hyperlattice-add` function:

```lisp
(defvar my-prob-hyperlattice (make-instance 'prob-hyperlattice))

(prob-hyperlattice-add my-prob-hyperlattice 'a 0.5)
(prob-hyperlattice-add my-prob-hyperlattice 'b 0.3)
(prob-hyperlattice-add my-prob-hyperlattice 'c 0.2)
```

This creates a new probabilistic hyperlattice called `my-prob-hyperlattice` with three elements: `a` with probability `0.5`, `b` with probability `0.3`, and `c` with probability `0.2`.

You can compute the supremum and infimum of two probabilistic lattices using the `prob-hyperlattice-sup` and `prob-hyperlattice-inf` functions:

```lisp
(defvar prob-lattice1 (make-instance 'prob-lattice))
(prob-lattice-add prob-lattice1 'a 0.6)
(prob-lattice-add prob-lattice1 'b 0.4)

(defvar prob-lattice2 (make-instance 'prob-lattice))
(prob-lattice-add prob-lattice2 'a 0.3)
(prob-lattice-add prob-lattice2 'c 0.7)

;; returns a new probabilistic hyperlattice with elements (a 0.6 b 0.3 c 0.2)
(prob-hyperlattice-sup my-prob-hyperlattice prob-lattice1)

;; returns a new probabilistic hyperlattice with elements (a 0.3)
(prob-hyperlattice-inf my-prob-hyperlattice prob-lattice2)
```

You can compute the supremum and infimum of a set of probabilistic lattices using the `prob-hyperlattice-sup-set` and `prob-hyperlattice-inf-set` functions:

```lisp
;; returns a new probabilistic hyperlattice with elements (a 0.18 b 0.12 c 0.14)
(prob-hyperlattice-sup-set my-prob-hyperlattice '(prob-lattice1 prob-lattice2))

;; returns a new probabilistic hyperlattice with elements (a 0.3)
(prob-hyperlattice-inf-set my-prob-hyperlattice '(prob-lattice1 prob-lattice2))
```

You can compute the closure of a set of probabilistic lattices using the `prob-hyperlattice-closure` function:

```lisp
;; returns (prob-lattice1 (a 0.5 b 0.3))
(prob-hyperlattice-closure my-prob-hyperlattice '(prob-lattice1))
```

This computes the closure of the set `(prob-lattice1)` in `my-prob-hyperlattice`, which is `(prob-lattice1 (a 0.5 b 0.3))`.