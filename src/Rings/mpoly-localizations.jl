export AbsMultSet, AbsComplementOfPrimeIdeal
export ComplementOfPrimeIdeal


export AbsLocalizedRing
export original_ring, inverted_set
export LocalizedRing

export AbsLocalizedRingElem

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
    abstract type AbsMultSet{RingElemType} end

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
    mutable struct LocalizedRing{RingType, RingElemType, MultSetType} <: AbsLocalizedRing{RingType, RingElemType, MultSetType} 

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

Part of the minimal interface for localized rings. Returns the original ring R of type `RingType` before localization.
"""
original_ring(W::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} = W.R

@Markdown.doc """
    original_ring(W::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} = W.R

Part of the minimal interface for localized rings. Returns the multiplicatively closed set S ⊂ R whose elements have been inverted to arrive at W = R[S⁻¹].
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
"""
abstract type AbsLocalizedRingElem{RingType, RingElemType, MultSetType} end

@Markdown.doc """
    mutable struct LocalizedRingElem{RingType, RingElemType, MultSetType}

A minimal implementation for elements f in a localized ring R[S⁻¹] where 
R is of type `RingType`, with elements of type `RingElemType`, and 
S of type `MultSetType`. 
"""
mutable struct LocalizedRingElem{RingType, RingElemType, MultSetType}
  f::AbstractAlgebra.Generic.Frac{RingElemType} # The element of the localized ring as a fraction with admissible denominator
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

@Markdown.doc """
    (W::LocalizedRing{RingType, RingElemType, MultSetType})(f::RingElemType) where {RingType, RingElemType, MultSetType}

Part of the minimal interface for localized rings. Suppose W = R[S⁻¹]; then this routine 
returns the conversion of an element f ∈ R to an element f//1 ∈ W.
"""
(W::LocalizedRing{RingType, RingElemType, MultSetType})(f::RingElemType) where {RingType, RingElemType, MultSetType} = LocalizedRingElem(f, W)

@Markdown.doc """
    (W::LocalizedRing{RingType, RingElemType, MultSetType})(i::Int) where {RingType, RingElemType, MultSetType}

Part of the minimal interface for localized rings. This routine returns the conversion of 
an integer i to an element i//1 ∈ W.
"""
(W::LocalizedRing{RingType, RingElemType, MultSetType})(i::Int) where {RingType, RingElemType, MultSetType} = W(original_ring(W)(i))

function +(a::LocalizedRingElem{RingType, RingElemType, MultSetType}, 
	   b::LocalizedRingElem{RingType, RingElemType, MultSetType}
	   ) where {RingType, RingElemType, MultSetType}
  a.R_loc == b.R_loc || error("the arguments do not have the same parent ring")
  return LocalizedRingElem{RingType, RingElemType, MultSetType}(a.f + b.f, a.R_loc)
end

function -(a::LocalizedRingElem{RingType, RingElemType, MultSetType}, 
	   b::LocalizedRingElem{RingType, RingElemType, MultSetType}
	   ) where {RingType, RingElemType, MultSetType}
  a.R_loc == b.R_loc || error("the arguments do not have the same parent ring")
  return LocalizedRingElem{RingType, RingElemType, MultSetType}(a.f - b.f, a.R_loc)
end

function *(a::LocalizedRingElem{RingType, RingElemType, MultSetType}, 
	   b::LocalizedRingElem{RingType, RingElemType, MultSetType}
	   ) where {RingType, RingElemType, MultSetType}
  a.R_loc == b.R_loc || error("the arguments do not have the same parent ring")
  return LocalizedRingElem{RingType, RingElemType, MultSetType}(a.f * b.f, a.R_loc)
end

function Base.:(//)(a::LocalizedRingElem{RingType, RingElemType, MultSetType}, 
	   b::LocalizedRingElem{RingType, RingElemType, MultSetType}
	   ) where {RingType, RingElemType, MultSetType}
  a.R_loc == b.R_loc || error("the arguments do not have the same parent ring")
  R_loc = a.R_loc
  numerator(b) in inverted_set(R_loc) || error("the second argument is not a unit in this local ring")
  return LocalizedRingElem{RingType, RingElemType, MultSetType}(a.f // b.f, a.R_loc)
end

function ==(a::LocalizedRingElem{RingType, RingElemType, MultSetType}, 
	   b::LocalizedRingElem{RingType, RingElemType, MultSetType}
	   ) where {RingType, RingElemType, MultSetType}
  a.R_loc == b.R_loc || error("the arguments do not have the same parent ring")
  return a.f == b.f 
end

function ^(a::LocalizedRingElem{RingType, RingElemType, MultSetType}, 
	   i::Int
	   ) where {RingType, RingElemType, MultSetType}
  return LocalizedRingElem{RingType, RingElemType, MultSetType}((a.f)^i, a.R_loc)
end
# TODO: Add the eventually missing usual arithmetic


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

**Note:** This needs the methods `isprime(::Ideal{RingElemType})` and `Base.in(::RingElemType, ::Ideal{RingElemType})` to be implemented!
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

Abstract type for honest local rings arising from localizations at prime ideals.
"""
abstract type AbsLocalRing{RingType, RingElemType} <: AbsLocalizedRing{RingType, RingElemType, AbsComplementOfPrimeIdeal{RingType, RingElemType}} end

############################################################################
# Localizations of polynomial rings over admissible fields at prime ideals #
############################################################################
@Markdown.doc """
    mutable struct MPolyLocalRing{BaseRingType} <: AbsLocalRing{MPolyRing{BaseRingType}, MPolyElem{BaseRingType}}

The localization of a multivariate polynomial ring R = 𝕜[x₁,…,xₙ] over a base field 𝕜 of type `BaseRingType` at a prime ideal P ⊂ R.
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

#@Markdown.doc """
    #mutable struct ComplementOfMaximalIdeal{RingElemType} <: AbsMultSet{RingElemType}  
#
#The multiplicatively closed set S given as the complement S = R \\ 𝔪 
#of a maximal ideal 𝔪 = ⟨x₁-a₁,…,xₙ-aₙ⟩ where R = 𝕜[x₁,…,xₙ] is a free polynomial 
#ring over a base field 𝕜.
#
#**Note:** This is not for *arbitrary* maximal ideals, but *only* for those of the form above, 
#i.e. those which stay maximal when passing to an algebraic closure of the base field 𝕜.
#"""
#mutable struct ComplementOfMaximalIdeal{RingType, RingElemType} <: AbsMultSet{RingType, RingElemType}  
#end
#
#function Base.in(f::RingElemType, S::ComplementOfMaximalIdeal{RingType, RingElemType}) where {RingType, RingElemType}
#  error( "not implemented" )
#end
#
#@Markdown.doc """
#    mutable struct PowersOfElement{RingType, RingElemType} <: AbsMultSet{RingType, RingElemType} 
#
#The multiplicatively closed set S of a ring R of type `RingType` given by the 
#powers S = { fᵏ : k ∈ ℤ } for some polynomial f ∈ R.
#"""
#mutable struct PowersOfElement{RingType, RingElemType} <: AbsMultSet{RingType, RingElemType} 
#  # essential fields
#  f::RingElemType
#
#  # fields used for caching
#  R::RingType # the parent of f
#
#  function PowersOfElement{RingType, RingElemType}(f::RingElemType) where {RingType, RingElemType}
#    S = new{RingType, RingElemType}(f, parent(f))
#    return S
#  end
#end

#function Base.in(f::RingElemType, S::PowersOfElement{RingType, RingElemType}) where {RingType, RingElemType}
  #error( "not implemented" )
#end
#
#
