# SRFI 134: Immutable deques

This is an egg (library) for [CHICKEN 5](https://call-cc.org/)
providing [SRFI 134](https://srfi.schemers.org/srfi-134) immutable
deques.  These are implemented by the "banker's deque" structure
described by Chris Okasaki in _Purely Functional Data Structures_;
these provide amortized O(1) running time for all basic deque
operations (`ideque-front`, `ideque-remove-back`, etc.).

# Documentation

See [the CHICKEN wiki page](https://wiki.call-cc.org/eggref/5/srfi-134).

# Author

SRFI 134 is by Kevin Wortman, John Cowan, and Shiro Kawai.

This implementation began life as the two-list sample implementation
written by Shiro Kawai.  It was rewritten and expanded by Wolfgang
Corcoran-Mathe.

# Maintainer

Wolfgang Corcoran-Mathe

# License

MIT/X
