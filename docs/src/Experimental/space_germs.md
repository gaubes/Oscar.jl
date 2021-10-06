```@meta
CurrentModule = Oscar
```

```@setup oscar
using Oscar
```

# Generalities on Space germs

The geometric notion of a space germ is a local concept. A space germ (X,x) at a point $x$ is an equivalence class of ringed spaces, each of which contains $x$ in its underlying topological space, and the equivalence relation is precisely the existence of an open neighbourhood of $x$ on which the spaces coincide.

Depending on the kind of ringed space in question, space germs arise in
different forms:

  * a space germ in the context of affine schemes is the geometric object arising from a given scheme by localization at a point, leading to the stalk of the structure sheaf at the respective prime ideal.

  * in the context of singularity theory, the (anti-)equivalence of categories between complex space germs and analytic ``{\mathbb C}``-algebras allows the direct definition of a space germ from algebraic data

Note that analytic algebras as mentioned above, have two computational problems.
On one hand, exact computations can only be performed over fields permitting
exact computations as e.g. ``{\mathbb Q}`` or ``{\mathbb F}_p`` or algebraic
extensions thereof, but not over ``{\mathbb R}`` or ``{\mathbb C}``. This
usually does not pose a problem, if the input data is in the smaller field. But
unfortunately, also analytic algebras are not computable so that applications
have to be considered either in a localization of a multivariate polynomial
ring or in terms of a multivariate formal power series using the following
inclusions:
```math
{\mathbb Q}[\underline{x}]_{\langle x \rangle} \hookrightarrow
  {\mathbb Q}\{\underline{x}\}\hookrightarrow
  {\mathbb Q}[[\underline{x}]]
```

Textbooks covering space germs in the sense of analytic algebras and
singularity theory are:
- [GLS07](@cite)
- [dJP00](@cite)

For the point of view of schemes, we refer to the page on schemes and the references given there; an example of a standard textbook is
- [Har77](@cite).

## Creating space germs in Oscar

In general, space germs in Oscar are created in the following ways:

  * localization of an affine scheme at a point
    ```julia
    SpaceGerm(S::AffineScheme, I::IdealSheaf)
    ```
    where I is an ideal sheaf of prime ideals describing the point on the
    scheme.
    
    **Provides**: Space germ, restriction map


  * localization of a polynomial ring at a point
    ```julia
    SpaceGerm(R::MPolyRing, I::MPolyIdeal)
    SpaceGerm(R::MPolyRingQuo, I::MPolyIdeal?)
    ```
    where I is either a maximal ideal describing a base_ring(R)-point or
    a prime ideal.   

    **Provides**: Space germ, restriction map


  * localized ring with S of type ComplemOfPrimeIdeal or ComplemOfMaxIdeal
    ```julia
    SpaceGerm(R::MPolyLocalRing)
    SpaceGerm(R::MPolyLocalRingQuo)
    ```
    **Provides**: Space germ, restriction map inherited from LocalizedRing


  * from a quotient of a formal power series ring
    ```julia
    SpaceGerm(R::??Potenzreihenring???, I::??Ideal???)
    ```
    **Provides**: Space germ   

    **Caution**: This variant SpaceGerm does not permit computations and
    should only be used, if explicit power series expansions up to
    given percision are desired.

Morphisms of space germs are morphisms of the underlying local rings and hence
also handled in this way in Oscar.

## Basic functionality for space germs

Most of the basic functionality immediately falls back to the underlying ring level and is just provided on the geometric side for convenience:

  * equality of space germs
    ```julia
    equal(R::SpaceGerm, T::SpaceGerm)
    ```
    **Caution**:
      + This is not available for space germs created from power series rings
      + This is only available, if the two germs are subgerms of a common larger germ.
    
  * representative of a space germ
    ```julia
    representative(R::SpaceGerm)
    ```
    **Provides**: Affine Scheme   

    **Caution**:
      + This is not available for space germs created from power series rings

  * intersection of space germs
    ```julia
    intersection(R::SpaceGerm,T::SpaceGerm)
    ```
    **Provides**: Space Germ   

    **Caution**:
      + This is only available, if the two germs are subgerms of a common larger germ.

  * union of space germs
    ```julia
    union(R::SpaceGerm,T::SpaceGerm)
    ```
    **Provides**: Space Germ   

    **Caution**:
      + This is only available, if the two germs are subgerms of a
        common larger germ.
      + This is not available for space germs created from power series rings
