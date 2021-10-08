```@meta
CurrentModule = Oscar
```

```@setup oscar
using Oscar
```

# Local rings in Oscar

Oscar provides several models for various local rings. To name a few, let ``R = \mathbb k[x_1,\dots,x_n]/I`` be some finitely generated algebra over a field ``\mathbb k`` for some ideal ``I \subset \mathbb k[x_1,\dots,x_n]``. By abuse of notation, we will identify ideals ``J \subset R`` with their preimages 
``J \subset \mathbb k[x_1,\dots,x_n]`` under the canonical projection. Then we have

* localizations ``R[f^{-1}]`` at a specific polynomial ``f \in \mathbb{k}[x_1,\dots,x_n]`` (respectively, its residue class in ``R``);

* localizations ``R_{\mathfrak m}`` at a maximal ideal ``\mathfrak m = \langle x_1-a_1,\dots,x_n-a_n\rangle`` with entries ``a_i \in \mathbb{k}``;

* localizations at prime ideals ``R_J`` with ``J \subset \mathbb{k}[x_1,\dots,x_n]`` a prime ideal containing ``I``;

* formal power series rings ``\mathbb k[[x_1,\dots,x_n]]`` and their quotients.

Depending on the case, these localizations and their algorithmic backend is realized in quite different ways. For instance, the localization of ``R`` as above at a given element ``f\in R`` can be implemented by adding an extra variable, i.e. using Rabinowitsch's trick:
```math
R[f^{-1}] \cong R[u]/\langle 1-u\cdot f\rangle.
```
Thus, this type of localization can be reduced to the use of affine algebras and their algorithmic backends.

The localizations of ``R`` at maximal ideals ``\mathfrak m = \langle x_1-a_1,\dots,x_n-a_n\rangle`` is merely a special case of the more general localization at the complement of prime ideals ``J \subset R``. However, the maximal ideal case is special in the following sense: Note that the ring 
```math
R_{\mathfrak m} = \left\{ \frac{f}{g} : f,g \in R, \, g \notin \mathfrak m \right\}
```
is *not* finitely generated as an algebra over the base field so that algorithms for polynomials can not be applied to it directly. Nevertheless, a lot of computations in ``R_{\mathfrak m}`` can be reduced to computations in ``\mathbb k[x_1,\dots,x_n]`` by translating the point ``(a_1,\dots,a_n) \in \mathbb{k}^n`` to the origin and using appropriately chosen *monomial orderings*. 

The average Oscar user is not supposed to take care of such technicalities themselves. But be aware that depending on the actual type of localization, different algorithmic backends and are being used. We will in the following discuss the use and also the realization of these backends in more detail, including information for the advanced user. 


## Adjunction of inverses using Rabinowitsch's trick

TBA.

## Localizations of affine algebras at maximal ideals

The basic data type for such localizations is 
```@docs
    MPolyRingLoc{T}
```
Elements of such localizations are realized as fractions:
```@docs
    MPolyElemLoc{T}
```
Accordingly, all the arithmetic of such elements is carried out regarding them as fractions of polynomials.

Note that this structure works only for localizations of free polynomial rings. By 
virtue of commutative algebra, this can be extended to comprise also arbitrary affine 
algebras ``R = \mathbb k[x_1,\dots,x_n]/I`` via the description of ideals ``I \subset \mathbb k[x_1,\dots,x_n]``. 
More generally, ideals ``I`` in the localization ``P_{\mathfrak m}`` are modeled by 
```@docs
    MPolyIdealLoc{S}
```
In order to create such an ideal ``I``, it suffices to provide a list of generators 
``I = \langle h_1,\dots,h_r\rangle``, ``h_i = f_i/g_i \in P_{\mathfrak m}``, for instance 
with a call to 
```@docs
    ideal(h::Vector{T}) where T <: MPolyElemLoc
```

The structure `BiPolyArrayLoc{S}` in `MPolyRingLoc{T}` above already yields at the 
binding of a singular polynomial ring in the backend:
```@docs
    BiPolyArrayLoc{S}
```
The stored data already suggests that, being units, the denominators of the 
generators of an ideal can be discarded. Any routines such as, for example, testing 
ideal membership ``f \in I \subset P_{\mathfrak m}``, will then fall back to 
standard basis computation in the singular ring on the backend. This ring can 
be invoked explicitly by calling
```@docs
    singular_ring_loc(R::MPolyRingLoc{T}; ord::Symbol = :negdegrevlex) where T
```

## Formal power series rings

Truncated formal power series rings are documented in [Power series models](@ref).
TODO: What about non-truncated ones?

# A localization framework for commutative rings

Suppose ``R`` is a commutative ring with unit and ``S \subset R`` is a *multiplicatively 
closed set* containing ``1 \in R``. Then we can form the *localization* of ``R`` at ``S``
```math
    R[S^{-1}] = \left\{ \frac{p}{q} : p,q \in R, \, q \in S \right\},
```
either as a subset of the quotient field of ``R`` (in case ``R`` is an integral domain), 
or in the total ring of fractions. See, for instance, [Eis95] for a general construction 
of localizations.

Oscar provides a general framework for such localizations, originally intended to be used 
with multivariate polynomial rings ``R`` over some base field ``\mathbb k``. 
The localizations of such rings are, in general, not finitely 
generated as algebras over ``\mathbb k``, for instance when localizing at some maximal 
ideal ``\mathfrak m \subset R``. However, many ideal- and module-theoretic questions in the localization 
``R[S^{-1}]``, such as e.g. the ideal membership, can be transformed to questions on 
ideals and modules over the original ring ``R`` and then solved using Groebner- or standard-basis 
algorithms. This makes it important to regard localizations ``R[S^{-1}]`` as rings with 
a history of creation from the original pair ``S \subset R``. 

## Localizations 

We now explain the abstract interface that needs to be implemented for any concrete 
instance of localized rings, starting with 
```@docs
    AbsMultSet{RingType, RingElemType}
```
The basic functionality that has to be implemented for any concrete type derived from 
this is to be able to check containment of elements via
```@docs
    in(f::RingElemType, S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}
```
This is supposed to be an extension of the methods of the function `Base.in`.

A localized ring should then be derived from 
```@docs
    AbsLocalizedRing{RingType, RingElemType, MultSetType}
```
For any concrete instance of this type the following methods must be implemented:
```@docs
    original_ring(W::AbsLocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} 
```
```@docs
    inverted_set(W::AbsLocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType}
```
Also, conversion of fractions to elements of localized rings must be implemented in the form 
`(W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(f::AbstractAlgebra.Generic.Frac{RingElemType}) where {RingType, RingElemType, MultSetType}`.
A minimal implementation of this interface is provided by `LocalizedRing`.

The *elements* of localized rings already mentioned above must be derived from 
```@docs
    AbsLocalizedRingElem{RingType, RingElemType, MultSetType}
```
For any concrete instance `F` of `AbsLocalizedRingElem` there must be the following 
methods:
```@docs
    fraction(f::T) where {T<:AbsLocalizedRingElem} 
```
```@docs
    parent(f::T) where {T<:AbsLocalizedRingElem}
```
A default version of the arithmetic is implemented based on `AbstractAlgebra.Generic.Frac`. 
Depending on the actual concrete instance, one might wish to provide more fine-tuned methods. 


## Local rings
When localizing at a prime ideal ``P \subset R`` (i.e. ``S = R\setminus P``), the 
localized ring ``R_P := R[S^{-1}]`` is actually a *local ring*, meaning that it 
has a unique maximal ideal ``P_P\subset R_P``. These rings are of great importance 
in algebraic geometry and deserve special attention. The underlying multiplicatively 
closed set is of the type
```@docs
    ComplementOfPrimeIdeal{RingType, RingElemType}
```
In addition to the above mentioned methods for multiplicatively closed sets, in this case the user 
also has to assure that the method `isprime(::Ideal{RingElemType}) -> Boolean` is implemented 
to check whether or not a given ideal is prime.

A local ring arising from localization at prime ideals is then derived from 
```@docs
    AbsLocalRing{RingType, RingElemType, MultSetType}
```
A minimal implementation of this in the case of multivariate polynomial rings is given by 
`MPolyLocalRing`.

Elements of such rings are of type 
```@docs
    AbsLocalRingElem{RingType, RingElemType}
```
As before, a minimal implementation of this interface is given for multivariate polynomial 
rings by `MPolyLocalRingElem`.
