# SRFI 134: Immutable deques

This is an egg (library) for [CHICKEN 5](https://call-cc.org/)
providing [SRFI 134](https://srfi.schemers.org/srfi-134) immutable
deques.

# Issues

This "two-list" implementation is the one used by Gauche.  It's an
"eager clone" of the banker's deque structure described by Chris
Okasaki, and thus does not have the same amortized running-time
guarantees.

# Author

Kevin Wortman, John Cowan, and Shiro Kawai.

# Maintainer

Wolfgang Corcoran-Mathe

# License

MIT/X
