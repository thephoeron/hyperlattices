# Lattices

## About

A lattice is a partially ordered set in which every two elements have a unique supremum (least upper bound) and a unique infimum (greatest lower bound). We can represent a lattice in Common Lisp using a hash table to store the elements and a function to compute the supremum and infimum.

## Usage

This implementation defines a package called `lattices`, which exports a class called `lattice` that represents an algebraic lattice with a hash table to store the elements, and functions to compute the supremum and infimum. It also defines functions to add, remove, and check if an element is in the lattice, as well as functions to compute the supremum, infimum, and closure of a set of elements.

You can create a new lattice by creating an instance of the `lattice` class and adding elements to it using the `lattice-add` function:

```lisp
(defvar my-lattice (make-instance 'lattice))

(lattice-add my-lattice 'bottom)
(lattice-add my-lattice 1)
(lattice-add my-lattice 2)
(lattice-add my-lattice 'top)
```

This creates a new lattice called `my-lattice` with four elements: `bottom`, `1`, `2`, and `top`.

You can compute the supremum and infimum of two elements using the `lattice-sup` and `lattice-inf` functions:

```lisp
(lattice-sup my-lattice 1 2)
=> 2

(lattice-inf my-lattice 1 2)
=> 1
```

You can compute the supremum and infimum of a set of elements using the `lattice-sup-set` and `lattice-inf-set` functions:

```lisp
(lattice-sup-set my-lattice '(1 2))
=> 2

(lattice-inf-set my-lattice '(1 2))
=> 1
```

You can compute the closure of a set of elements using the `lattice-closure` function:

```lisp
(lattice-closure my-lattice '(1))
=> (1 bottom)
```

This computes the closure of the set `(1)` in `my-lattice`, which is `(1 bottom)`.