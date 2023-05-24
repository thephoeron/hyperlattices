# Hyperlattices

## About

A hyperlattice is a generalization of a lattice in which the elements are themselves lattices. We can represent a hyperlattice in Common Lisp using a hash table to store the elements and a function to compute the supremum and infimum.

## Usage

This library defines a class called `hyperlattice` that represents a hyperlattice with a hash table to store the elements, and functions to compute the supremum and infimum. It also defines functions to add, remove, and check if a lattice is in the hyperlattice, as well as functions to compute the supremum, infimum, and closure of a set of lattices.

You can create a new hyperlattice by creating an instance of the `hyperlattice` class and adding lattices to it using the `hyperlattice-add` function:

```lisp
(defvar my-hyperlattice (make-instance 'hyperlattice))

(defvar lattice1 (make-instance 'lattice))
(lattice-add lattice1 'bottom)
(lattice-add lattice1 1)
(lattice-add lattice1 2)
(lattice-add lattice1 'top)

(defvar lattice2 (make-instance 'lattice))
(lattice-add lattice2 'bottom)
(lattice-add lattice2 3)
(lattice-add lattice2 4)
(lattice-add lattice2 'top)

(hyperlattice-add my-hyperlattice lattice1)
(hyperlattice-add my-hyperlattice lattice2)
```

This creates a new hyperlattice called `my-hyperlattice` with two lattices: `lattice1` and `lattice2`.

You can compute the supremum and infimum of two lattices using the `hyperlattice-sup` and `hyperlattice-inf` functions:

```lisp
;; returns a new hyperlattice with elements (bottom 1 2 3 4 top)
(hyperlattice-sup my-hyperlattice lattice1 lattice2)

;; returns a new hyperlattice with elements (bottom top)
(hyperlattice-inf my-hyperlattice lattice1 lattice2)
```

You can compute the supremum and infimum of a set of lattices using the `hyperlattice-sup-set` and `hyperlattice-inf-set` functions:

```lisp
;; returns a new hyperlattice with elements (bottom 1 2 3 4 top)
(hyperlattice-sup-set my-hyperlattice '(lattice1 lattice2))

;; returns a new hyperlattice with elements (bottom top)
(hyperlattice-inf-set my-hyperlattice '(lattice1 lattice2))
```

You can compute the closure of a set of lattices using the `hyperlattice-closure` function:

```lisp
;; returns (lattice1 (bottom 1 2 top))
(hyperlattice-closure my-hyperlattice '(lattice1))
```

This computes the closure of the set `(lattice1)` in `my-hyperlattice`, which is `(lattice1 (bottom 1 2 top))`.