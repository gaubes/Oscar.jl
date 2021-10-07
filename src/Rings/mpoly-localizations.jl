export AbsMultSet, AbsComplementOfPrimeIdeal
export ComplementOfPrimeIdeal


export AbsLocalizedRing
export original_ring, inverted_set
export LocalizedRing

export AbsLocalizedRingElem
export fraction, parent

export AbsLocalRing
export MPolyLocalRing

import AbstractAlgebra.Ring

#################################################################################
# General framework for localizations of rings; to be used with affine algebras #
#################################################################################

#################################################################################
# Multiplicatively closed sets of (commutative) rings                           #
#################################################################################
@Markdown.doc """
    abstract type AbsMultSet{RingType, RingElemType} end

Abstract for a multiplicatively closed set in a commutative (Noetherian) ring 
R with elements of type `RingElemType`.
"""
abstract type AbsMultSet{RingType, RingElemType} end

@Markdown.doc """
    function Base.in(f::RingElemType, S<:AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}

Part of the required interface for the implementation of concrete types of multiplicatively 
closed sets S of type `<:AbsMultSet{RingType, RingElemType}` in a ring R of type `RingType`. 

Must return `true` if `f` belongs to `S`; `false` otherwise.

**Note:** If this routine is not implemented, the function call will default to the 
execution of an error message. 
"""
function Base.in(f::RingElemType, S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}
  error("method not implemented for multiplicatively closed sets of type $(typeof(S))")
end

#################################################################################
# Localizations of (commutative) rings at multiplicatively closed sets          #
#################################################################################
@Markdown.doc """
    abstract type AbsLocalizedRing{RingType, RingElemType, MultSetType} <: Ring end

The localization R[S⁻¹] of a ring R of type `RingType` with elements of type `RingElemType` at a 
multiplicatively closed set S of type `MultSetType`. 

In general, the arithmetic of such a localized ring R[S⁻¹] should be implemented using fractions 
of elements in the original ring R. The methods provided for the multiplicatively closed set S 
can be used to check whether a given denominator is admissible for the specific localization. 

Depending on the actual type of R and S, further functionality can then be provided using 
different Groebner-basis driven backends. 
"""
abstract type AbsLocalizedRing{RingType, RingElemType, MultSetType} <: Ring end

@Markdown.doc """
    original_ring(W::AbsLocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} 

Returns the original ring R for a localized ring of the form W = R[S⁻¹].
"""
function original_ring(W::AbsLocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} 
  error("`original_ring` is not implemented for localized rings of type $(typeof(W))")
end

@Markdown.doc """
    inverted_set(W::AbsLocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType}

Returns the set S of at which has been localized for a localized ring W = R[S⁻¹].
"""
function inverted_set(W::AbsLocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType}
  error("`inverted_set` is not implemented for localized rings of type $(typeof(W))")
end

@Markdown.doc """
    function (W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(f::AbstractAlgebra.Generic.Frac{RingElemType}) where {RingType, RingElemType, MultSetType} 

Converts a fraction f = a//b to an element of the localized ring W.
"""
function (W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(f::AbstractAlgebra.Generic.Frac{RingElemType}) where {RingType, RingElemType, MultSetType} 
  error("conversion for fractions to elements of type $(typeof(W)) is not implemented")
end

#################################################################################
# A minimal implementation of the above abstract interface                      #
#################################################################################
@Markdown.doc """
    LocalizedRing{RingType, RingElemType, MultSetType} <: AbsLocalizedRing{RingType, RingElemType, MultSetType} 

A minimal implementation for the localization R[S⁻¹] of a ring R of type `RingType` with 
elements of type `RingElemType`, localized at a multiplicatively closed set S of 
type `MultSetType`.
"""
mutable struct LocalizedRing{RingType, RingElemType, MultSetType} <: AbsLocalizedRing{RingType, RingElemType, MultSetType} 
  R::RingType # The parent ring which is being localized
  S::MultSetType # The multiplicatively closed set S ⊂ R whose inverses are added to R

  function LocalizedRing(R::RingType, S::MultSetType) where {RingType, MultSetType}
    # Sanity check whether the multiplicatively closed set S is compatible with the 
    # given rings
    MultSetType <: AbsMultSet{RingType, elem_type(RingType)} || error(
	"The type of the multiplicatively closed set is not compatible with the type of the ring"
	)
    R_loc = new{RingType, elem_type(RingType), MultSetType}(R,S)
    return R_loc
  end
end

@Markdown.doc """
    original_ring(W::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} = W.R

Returns the original ring R of type `RingType` before localization.
"""
original_ring(W::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} = W.R

@Markdown.doc """
    inverted_set(W::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} = W.R

Returns the multiplicatively closed set S ⊂ R whose elements have been inverted to arrive at W = R[S⁻¹].
"""
inverted_set(W::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} = W.S

#################################################################################
# Elements of localized rings                                                   #
#################################################################################
@Markdown.doc """
    abstract type AbsLocalizedRingElem{RingType, RingElemType, MultSetType}

The abstract type of an element of the localization R[S⁻¹] of a commutative ring 
R of type `RingType` with elements of type `RingElemType` at a multiplicatively 
closed set S of type `MultSetType`.

For any concrete instance `F` of `AbsLocalizedRingElem` there must be the following 
methods:
 * `fraction(::AbsLocalizedRingElem) -> <:AbstractAlgebra.Generic.Frac` returning the actual value of the element as a fraction in the localized ring;
 * `parent(::AbsLocalizedRingElem) -> <:AbsLocalizedRing` returning the parent ring of the element.
"""
abstract type AbsLocalizedRingElem{RingType, RingElemType, MultSetType} end

@Markdown.doc """
    function fraction(f::T) where {T<:AbsLocalizedRingElem} 

Returns the actual value of `f` as a fraction in the localization of the original ring.

**Note:** This must be implemented for any concrete type of `AbsLocalizedRingElem`!
"""
function fraction(f::T) where {T<:AbsLocalizedRingElem} 
  error("`fraction` is not implemented for the type $(typeof(f))")
end

@Markdown.doc """
    function parent(f::T) where {T<:AbsLocalizedRingElem}

Returns the parent ring R[S⁻¹] of `f`.

**Note:** This must be implemented for any concrete type of `AbsLocalizedRingElem`!
"""
function parent(f::T) where {T<:AbsLocalizedRingElem}
  error("`parent` is not implemented for the type $(typeof(f))")
end

function +(a::AbsLocalizedRingElem{RingType, RingElemType, MultSetType}, b::AbsLocalizedRingElem{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  return (parent(a))(fraction(a) + fraction(b))
end

function -(a::AbsLocalizedRingElem{RingType, RingElemType, MultSetType}, b::AbsLocalizedRingElem{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  return (parent(a))(fraction(a) - fraction(b))
end

function *(a::AbsLocalizedRingElem{RingType, RingElemType, MultSetType}, b::AbsLocalizedRingElem{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  return (parent(a))(fraction(a) * fraction(b))
end

function Base.:(//)(a::AbsLocalizedRingElem{RingType, RingElemType, MultSetType}, b::AbsLocalizedRingElem{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  numerator(fraction(b)) in inverted_set(parent(b)) || error("the second argument is not a unit in this local ring")
  return (parent(a))(fraction(a) // fraction(b))
end

function ==(a::T, b::T) where {T<:AbsLocalizedRingElem}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  return parent(a)(fraction(a) == fraction(b))
end

function ^(a::T, i::Int) where {T<:AbsLocalizedRingElem}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  return parent(a)(fraction(a)^i)
end

#################################################################################
# A minimal implementation of elements of localized rings                       #
#################################################################################
@Markdown.doc """
    mutable struct LocalizedRingElem{RingType, RingElemType, MultSetType}

A minimal implementation for elements f in a localized ring R[S⁻¹] where 
R is of type `RingType`, with elements of type `RingElemType`, and 
S of type `MultSetType`. 
"""
mutable struct LocalizedRingElem{RingType, RingElemType, MultSetType}
  frac::AbstractAlgebra.Generic.Frac{RingElemType} # The element of the localized ring as a fraction with admissible denominator
  R_loc::LocalizedRing{RingType, RingElemType, MultSetType} # the parent ring before localization 

  function LocalizedRingElem(f::AbstractAlgebra.Generic.Frac{RingElemType}, R_loc::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType}
    # Perform some sanity checks
    RingType == parent_type(RingElemType) || error(
	"The given type of ring is incompatible with the given element type"
	)
    MultSetType <: AbsMultSet{RingType, RingElemType} || error(
	"The given type of the multiplicatively closed set is incompatible with the given ring types"
	)
    return new{RingType, RingElemType, MultSetType}(f, R_loc)
  end
end

# The required interface
function parent(f::LocalizedRingElem{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} 
  return f.R_loc
end

function frac(f::LocalizedRingElem{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType}
  return f.frac
end

# Automatic conversion
@Markdown.doc """
    (W::LocalizedRing{RingType, RingElemType, MultSetType})(f::RingElemType) where {RingType, RingElemType, MultSetType}

Part of the minimal interface for localized rings. Suppose W = R[S⁻¹]; then this routine 
returns the conversion of an element f ∈ R to an element f//1 ∈ W.
"""
(W::LocalizedRing{RingType, RingElemType, MultSetType})(f::RingElemType) where {RingType, RingElemType, MultSetType} = LocalizedRingElem((FractionField(original_ring(W)))(f), W)

@Markdown.doc """
    (W::LocalizedRing{RingType, RingElemType, MultSetType})(i::Int) where {RingType, RingElemType, MultSetType}

Part of the minimal interface for localized rings. This routine returns the conversion of 
an integer i to an element i//1 ∈ W.
"""
(W::LocalizedRing{RingType, RingElemType, MultSetType})(i::Int) where {RingType, RingElemType, MultSetType} = W(original_ring(W)(i))

#################################################################
# Honest local rings arising from localizations at prime ideals #
#################################################################

@Markdown.doc """
    abstract type AbsComplementOfPrimeIdeal{RingType, RingElemType} <:AbsMultSet{RingType, RingElemType} end

Abstract type for the multiplicatively closed sets S = R \\ P for prime ideals P in a commutative ring R. 
This is comprises both the complement of maximal ideals, as well as arbitrary prime ideals. 
In general, one expects different algorithmic backends for each one of the two cases. This will lead to a 
distinction of the associated concrete types.
"""
abstract type AbsComplementOfPrimeIdeal{RingType, RingElemType} <:AbsMultSet{RingType, RingElemType} end

@Markdown.doc """
    mutable struct ComplementOfPrimeIdeal{RingType, RingElemType} <: AbsMultSet{RingType, RingElemType}

The multiplicatively closed set S given as the complement S = R ∖ P
of a prime ideal P in a multivariate polynomial ring R over some base field.

**Note:** This needs the the following methods to be implemented:
 * `isprime(::Ideal{RingElemType}) -> Boolean` to check whether the given ideal is prime;
 * `Base.in(::RingElemType, ::Ideal{RingElemType}) -> Boolean` to check whether an element belongs to the ideal.
"""
mutable struct ComplementOfPrimeIdeal{RingType, RingElemType} <: AbsComplementOfPrimeIdeal{RingType, RingElemType}
  # essential fields
  P::Ideal{RingElemType}

  # fields for caching
  R::RingType # the base ring 

  function ComplementOfPrimeIdeal(P::Ideal{RingElemType}) where {RingElemType}
    S = new{parent_type(RingElemType), RingElemType}(P, base_ring(P))
    isprime(S.P) || error( "the given ideal is not prime" )
    return S
  end
end

function Base.in(f::RingElemType, S::ComplementOfPrimeIdeal{RingType, RingElemType}) where {RingType, RingElemType}
  return !(f in S.P)
end

@Markdown.doc """
    abstract type AbsLocalRing{RingType, RingElemType, MultSetType} <: AbsLocalizedRing{RingType, RingElemType, MultSetType} end

Abstract type for local rings arising from localizations at prime ideals.
"""
abstract type AbsLocalRing{RingType, RingElemType} <: AbsLocalizedRing{RingType, RingElemType, AbsComplementOfPrimeIdeal{RingType, RingElemType}} end

original_ring(W::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} = W.R

@Markdown.doc """
    abstract type AbsLocalRingElem{RingType, RingElemType} <: AbsLocalizedRingElem{RingType, RingElemType, AbsComplementOfPrimeIdeal{RingType, RingElemType} end

Abstract type for elements of local rings arising from localizations at prime ideals.
"""
abstract type AbsLocalRingElem{RingType, RingElemType} <: AbsLocalizedRingElem{RingType, RingElemType, AbsComplementOfPrimeIdeal{RingType, RingElemType}} end

############################################################################
# Localizations of polynomial rings over admissible fields at prime ideals #
############################################################################
@Markdown.doc """
    mutable struct MPolyLocalRing{BaseRingType, RingType, RingElemType} <: AbsLocalRing{MPolyRing{BaseRingType}, MPolyElem{BaseRingType}}

The localization of a multivariate polynomial ring R = 𝕜[x₁,…,xₙ] of type `RingType` over a base field 𝕜 of type `BaseRingType` and with elements of type `RingElemType` at a prime ideal P ⊂ R.
"""
mutable struct MPolyLocalRing{BaseRingType, RingType, RingElemType} <: AbsLocalRing{MPolyRing{BaseRingType}, MPolyElem{BaseRingType}}
  R::RingType # The parent ring which is being localized
  S::ComplementOfPrimeIdeal{RingType, RingElemType} 

  function MPolyLocalRing(R::RingType, 
      S::ComplementOfPrimeIdeal{RingType, RingElemType}
    ) where {BaseRingType, RingType<:MPolyRing{BaseRingType}, RingElemType}
    # TODO: Add some sanity checks here?
    R_loc = new{BaseRingType, RingType, RingElemType}(R,S)
    return R_loc
  end
end

function MPolyLocalRing( R::MPolyRing{BaseRingType}, P::Ideal{RingElemType} ) where {BaseRingType, RingElemType}
  return MPolyLocalRing(R, ComplementOfPrimeIdeal(P))
end

function original_ring(W::MPolyLocalRing{BaseRingType, RingType, RingElemType}) where {BaseRingType, RingType, RingElemType}
  return W.R
end

function inverted_set(W::MPolyLocalRing{BaseRingType, RingType, RingElemType}) where {BaseRingType, RingType, RingElemType}
  return W.S
end

mutable struct MPolyLocalRingElem{BaseRingType, RingType, RingElemType} <: AbsLocalRingElem{MPolyRing{BaseRingType}, MPolyElem{BaseRingType}} 
  frac::AbstractAlgebra.Generic.Frac{RingElemType}
  R_loc::MPolyLocalRing{BaseRingType, RingType, RingElemType}

  function MPolyLocalRingElem(f::AbstractAlgebra.Generic.Frac{RingElemType}, R_loc::MPolyLocalRing{BaseRingType, RingType, RingElemType}) where {BaseRingType, RingType, RingElemType}

    base_ring(parent(f)) == original_ring(R_loc) || error("the numerator and denominator of the given fraction do not belong to the original ring before localization")
    denominator(f) in inverted_set(R_loc) || error("the given denominator is not admissible for this localization")
    return new{BaseRingType, RingType, RingElemType}(f, R_loc)
  end
end

fraction(a::MPolyLocalRingElem{BaseRingType, RingType, RingElemType}) where {BaseRingType, RingType, RingElemType} = a.frac

parent(a::MPolyLocalRingElem{BaseRingType, RingType, RingElemType}) where {BaseRingType, RingType, RingElemType} = a.R_loc

(W::MPolyLocalRing{BaseRingType, RingType, RingElemType})(f::RingElemType) where {BaseRingType, RingType, RingElemType} = MPolyLocalRingElem((FractionField(original_ring(W)))(f), W)

(W::MPolyLocalRing{BaseRingType, RingType, RingElemType})(f::AbstractAlgebra.Generic.Frac{RingElemType}) where {BaseRingType, RingType, RingElemType} = MPolyLocalRingElem(f, W)
