```@meta
CurrentModule = Oscar
```

```@setup oscar
using Oscar
```

# Space germs (see GLS Seite 55-58)

The geometric notion of a space germ is a local concept. A space germ at a
point $x$ is an equivalence class of ringed spaces, each of which contains
$x$ in its underlying topological space, and the equivalence relation is
precisely the existence of an open neighbourhood of $x$ on which the spaces
coincide.

Depending on the kind of ringed space in question, space germs arise in
different forms:

* a space germ in the context of affine schemes is the geometric object
arising from a given scheme by localization at a point, leading to the
stalk of the structure sheaf at the respective prime ideal.

* in the context of singularity theory, the (anti-)equivalence of categories
between complex space germs and analytic ``{\mathbb C}``-algebras allows the
direct definition of a space germ from algebraic data

Note that analytic algebras as mentioned above, have two computational problems.
On one hand, exact computations can only be performed over fields permitting
exact computations as e.g. ``{\mathbb Q}`` or ``{\mathbb F}_p`` or algebraic
extensions thereof, but not over ``{\mathbb R}`` or ``{\mathbb C}``. This
usually does not pose a problem, if the input data is in the smaller field. But
unfortunately, also analytic algebras are not computable so that applications
have to be considered either in a localization of a polynomial ring or in terms
of formal power series using the following inclusions:
``{\mathbb Q}[\underline{x}]_{\langle x \rangle \hookrightarrow
  {\mathbb Q}\{\underline{x}\}\hookrightarrow
  {\mathbb Q}[[\underline{x}]]``

# Constructions of space germs

* from localization of a scheme at a point

* from localization of a polynomial ring at a point

* from a sub-quo of a formal power series

# Morphisms of space germs

Morphisms of space germs are morphisms of the underlying local rings and hence
also handled in this way in Oscar.

