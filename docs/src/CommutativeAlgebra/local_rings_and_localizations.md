```@meta
CurrentModule = Oscar
```

```@setup oscar
using Oscar
```

# Local rings in Oscar

Oscar provides several models for various local rings. To name a few, let ``R = \mathbb k[x_1,\dots,x_n]/I`` be some finitely generated algebra over a field ``\mathbb k`` for some ideal ``I \subset \mathbb k[x_1,\dots,x_n]``. By abuse of notation, we will identify ideals ``J \subset R`` with their preimages 
``J \subset \mathbb k[x_1,\dots,x_n]`` under the canonical projection. Then we have


* localizations of polynomial rings ``R_{\mathfrak m}`` at a maximal ideal ``\mathfrak m = \langle x_1-a_1,\dots,x_n-a_n\rangle`` with entries ``a_i \in \mathbb{k}``;

* localizations of affine algebras ``R[f^{-1}]`` at a specific polynomial ``f \in \mathbb{k}[x_1,\dots,x_n]`` (respectively, its residue class in ``R``);

* localizations at the complements of prime ideals ``R_P`` with ``P \subset \mathbb{k}[x_1,\dots,x_n]`` a prime ideal containing ``I``;

* formal power series rings ``\mathbb k[[x_1,\dots,x_n]]`` and their quotients;

Depending on the case, these localizations and their algorithmic backend is realized in quite different ways. For instance, the localization of an affine algebra ``R = \mathbb k[x_1,\dots,x_n]/I`` at a given element ``f\in R`` can be implemented by adding an extra variable using Rabinowitsch's trick
```math
R[f^{-1}] \cong R[u]/\langle 1-u\cdot f\rangle.
```
The localizations of affine algebras ``R`` as above at (the complement of) maximal ideals ``\mathfrak m = \langle x_1-a_1,\dots,x_n-a_n\rangle`` is merely a special case of the more general localization at the complement of prime ideals ``P \subset R``. However, the maximal ideal case is special in the sense that in the backend, a lot of computations in 
```math
R_{\mathfrak m} = \left\{ \frac{f}{g} : f,g \in R, \quad g \notin \mathfrak m \right\},
```
which is *not* finitely generated as an algebra over the base field, can be reduced to computations 
in ``\mathbb k[x_1,\dots,x_n]`` using appropriately chosen *monomial orderings*.
