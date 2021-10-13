export AbsMultSet, AbsComplementOfPrimeIdeal
export ComplementOfPrimeIdeal, PowersOfElement


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
    AbsMultSet{RingType, RingElemType}

Abstract type for a multiplicatively closed set in a commutative (Noetherian) ring 
R of type `RingType` with elements of type `RingElemType`.
"""
abstract type AbsMultSet{RingType, RingElemType} end

@Markdown.doc """
    in(f::RingElemType, S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}

Returns `true` if `f` belongs to `S`; `false` otherwise.

**Note:** If this routine is not implemented, the function call will default to the 
execution of an error message. 
"""
function Base.in(f::RingElemType, S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}
  error("method not implemented for multiplicatively closed sets of type $(typeof(S))")
end

@Markdown.doc """
    AbsPowersOfElement{RingType, RingElemType} <: AbsMultSet{RingType, RingElemType}

Abstract type for the multiplicatively closed set ``S = { fᵏ : f ∈ R, k ∈ ℕ₀ } ⊂ R`` 
for some element ``f`` of type `RingElemType` in a ring ``R`` of type `RingType`.
"""
abstract type AbsPowersOfElement{RingType, RingElemType} <: AbsMultSet{RingType, RingElemType} end

@Markdown.doc """
    generator(P::AbsPowersOfElement{RingType, RingElemType}) where {RingType, RingElemType}

Returns the element ``f`` for ``P = { fᵏ : f ∈ R, k ∈ ℕ₀ }``.
"""
function generator(P::AbsPowersOfElement{RingType, RingElemType}) where {RingType, RingElemType}
  error("method `generator(P::AbsPowersOfElement{RingType, RingElemType})` not implemented for `P` of type $(typeof(P))")
end

function Base.in(g::RingElemType, P::AbsPowersOfElement{RingType, RingElemType}) where {RingType, RingElemType}
  # WARNING: This is a very inefficient catchall implementation!
  parent(g) == parent(f) || error("elements do not belong to the same parent ring")
  while(!(g==one(parent(g))))
    (b, g) = divides(g, generator(P)) 
    if(b==false) 
      return false
    end
  end
  return true
end

@Markdown.doc """
    PowersOfElement{RingType, RingElemType}

A minimal implementation of `AbsPowersOfElement{RingType, RingElemType}`.
"""
mutable struct PowersOfElement{RingType, RingElemType} <: AbsPowersOfElement{RingType, RingElemType}
  R::RingType 
  f::RingElemType

  function PowersOfElement(f::RingElemType) where {RingElemType} 
    return new{parent_type(f), RingElemType}(parent(f), f)
  end
end

function generator(P::PowersOfElement{RingType, RingElemType}) where {RingType, RingElemType}
  return P.f
end

@Markdown.doc """
    AbsComplementOfPrimeIdeal{RingType, RingElemType}

Abstract type for the multiplicatively closed sets S = R \\ P for prime ideals P in a commutative ring R. 
This is comprises both the complement of maximal ideals, as well as arbitrary prime ideals. 
In general, one expects different algorithmic backends for each one of the two cases. This will lead to a 
distinction of the associated concrete types.
"""
abstract type AbsComplementOfPrimeIdeal{RingType, RingElemType} <:AbsMultSet{RingType, RingElemType} end

function Base.in(f::RingElemType, S::AbsComplementOfPrimeIdeal{RingType, RingElemType}) where {RingType, RingElemType}
  return !(f in S.P)
end

@Markdown.doc """
    ComplementOfPrimeIdeal{RingType, RingElemType}

The multiplicatively closed set S given as the complement S = R ∖ P
of a prime ideal P in a commutative ring R of type `RingType` with elements 
of type `RingElemType`.
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

#################################################################################
# Localizations of (commutative) rings at multiplicatively closed sets          #
#################################################################################
@Markdown.doc """
    AbsLocalizedRing{RingType, RingElemType, MultSetType}

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
    (W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(f::AbstractAlgebra.Generic.Frac{RingElemType}) where {RingType, RingElemType, MultSetType} 

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

original_ring(W::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} = W.R

inverted_set(W::LocalizedRing{RingType, RingElemType, MultSetType}) where {RingType, RingElemType, MultSetType} = W.S

#################################################################################
# Elements of localized rings                                                   #
#################################################################################
@Markdown.doc """
    AbsLocalizedRingElem{RingType, RingElemType, MultSetType}

The abstract type of an element of the localization R[S⁻¹] of a commutative ring 
R of type `RingType` with elements of type `RingElemType` at a multiplicatively 
closed set S of type `MultSetType`.
"""
abstract type AbsLocalizedRingElem{RingType, RingElemType, MultSetType} end

@Markdown.doc """
    fraction(f::T) where {T<:AbsLocalizedRingElem} 

Returns the actual value of `f` as a fraction in the localization of the original ring.
"""
function fraction(f::T) where {T<:AbsLocalizedRingElem} 
  error("`fraction` is not implemented for the type $(typeof(f))")
end

@Markdown.doc """
    parent(f::T) where {T<:AbsLocalizedRingElem}

Returns the parent ring R[S⁻¹] of `f`.
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
# Localizations of polynomial rings                             #
#################################################################

@Markdown.doc """
    MPolyLocalizedRing{BaseRingType, RingElemType, MultSetType} <: AbsLocalizedRing{MPolyRing{BaseRingType}, MPolyElem{BaseRingType}}

Localizations ``R[S⁻¹]`` of free polynomial rings ``R = 𝕜[x₁,…,xₙ]`` with elements of type 
`RingElemType` over some base ring ``𝕜`` of type `BaseRingType` and with multiplicatively 
closed set ``S`` of type `MultSetType`.
"""
mutable struct MPolyLocalizedRing{BaseRingType, RingElemType, MultSetType} <: AbsLocalizedRing{MPolyRing{BaseRingType}, MPolyElem{BaseRingType}, MultSetType}
  R::MPolyRing{BaseRingType} # The parent ring which is being localized
  S::MultSetType

  function MPolyLocalRing(R::MPolyRing{BaseRingType}, 
      S::MultSetType) where {BaseRingType, RingElemType, MultSetType}
    # TODO: Add some sanity checks here?
    R_loc = new{BaseRingType, RingElemType, MultSetType}(R,S)
    return R_loc
  end
end

#################################################################
# Honest local rings arising from localizations at prime ideals #
#################################################################

@Markdown.doc """
    AbsLocalRing{RingType, RingElemType, MultSetType}

Abstract type for local rings arising from localizations at prime ideals.
"""
abstract type AbsLocalRing{RingType, RingElemType} <: AbsLocalizedRing{RingType, RingElemType, AbsComplementOfPrimeIdeal{RingType, RingElemType}} end

function original_ring(W::AbsLocalRing{RingType, RingElemType}) where {RingType, RingElemType}
  error("`original_ring` not implemented for local rings of type $(typeof(W))")
end

function inverted_set(W::AbsLocalRing{RingType, RingElemType}) where {RingType, RingElemType}
  error("`inverted_set` not implemented for local rings of type $(typeof(W))")
end

@Markdown.doc """
    AbsLocalRingElem{RingType, RingElemType} <: AbsLocalizedRingElem{RingType, RingElemType, AbsComplementOfPrimeIdeal{RingType, RingElemType}

Abstract type for elements of local rings arising from localizations at prime ideals.
"""
abstract type AbsLocalRingElem{RingType, RingElemType} <: AbsLocalizedRingElem{RingType, RingElemType, AbsComplementOfPrimeIdeal{RingType, RingElemType}} end

############################################################################
# Localizations of polynomial rings over admissible fields at prime ideals #
############################################################################
@Markdown.doc """
    mutable struct MPolyLocalRing{BaseRingType, RingElemType} <: AbsLocalRing{MPolyRing{BaseRingType}, MPolyElem{BaseRingType}}

The localization of a multivariate polynomial ring R = 𝕜[x₁,…,xₙ] of type `RingType` over a base field 𝕜 of type `BaseRingType` and with elements of type `RingElemType` at a prime ideal P ⊂ R.
"""
mutable struct MPolyLocalRing{BaseRingType, RingElemType} <: AbsLocalRing{MPolyRing{BaseRingType}, MPolyElem{BaseRingType}}
  R::MPolyRing{BaseRingType} # The parent ring which is being localized
  S::ComplementOfPrimeIdeal{MPolyRing{BaseRingType}, RingElemType} 

  function MPolyLocalRing(R::MPolyRing{BaseRingType}, 
      S::ComplementOfPrimeIdeal{MPolyRing{BaseRingType}, RingElemType}
    ) where {BaseRingType, RingElemType}
    # TODO: Add some sanity checks here?
    R_loc = new{BaseRingType, RingElemType}(R,S)
    return R_loc
  end
end

function MPolyLocalRing( R::MPolyRing{BaseRingType}, P::Ideal{RingElemType} ) where {BaseRingType, RingElemType}
  return MPolyLocalRing(R, ComplementOfPrimeIdeal(P))
end

function original_ring(W::MPolyLocalRing{BaseRingType, RingElemType}) where {BaseRingType, RingElemType}
  return W.R
end

function inverted_set(W::MPolyLocalRing{BaseRingType, RingElemType}) where {BaseRingType, RingElemType}
  return W.S
end

mutable struct MPolyLocalRingElem{BaseRingType, RingElemType} <: AbsLocalRingElem{MPolyRing{BaseRingType}, MPolyElem{BaseRingType}} 
  frac::AbstractAlgebra.Generic.Frac{RingElemType}
  R_loc::MPolyLocalRing{BaseRingType, RingElemType}

  function MPolyLocalRingElem(f::AbstractAlgebra.Generic.Frac{RingElemType}, R_loc::MPolyLocalRing{BaseRingType, RingElemType}) where {BaseRingType, RingElemType}

    base_ring(parent(f)) == original_ring(R_loc) || error("the numerator and denominator of the given fraction do not belong to the original ring before localization")
    denominator(f) in inverted_set(R_loc) || error("the given denominator is not admissible for this localization")
    return new{BaseRingType, RingElemType}(f, R_loc)
  end
end

fraction(a::MPolyLocalRingElem{BaseRingType, RingElemType}) where {BaseRingType, RingElemType} = a.frac

parent(a::MPolyLocalRingElem{BaseRingType, RingElemType}) where {BaseRingType, RingElemType} = a.R_loc

(W::MPolyLocalRing{BaseRingType, RingElemType})(f::RingElemType) where {BaseRingType, RingElemType} = MPolyLocalRingElem((FractionField(original_ring(W)))(f), W)

(W::MPolyLocalRing{BaseRingType, RingElemType})(f::AbstractAlgebra.Generic.Frac{RingElemType}) where {BaseRingType, RingElemType} = MPolyLocalRingElem(f, W)

############################################################################
# Ideals in localized rings                                                #
############################################################################

@Markdown.doc """
    AbsLocalizedIdeal{RingType, RingElemType, MultSetType} <: Ideal{RingElemType}

Abstract type for ideals ``IS⁻¹ ⊂ R[S⁻¹]`` in localized rings. In fact, every 
such ideal is of this form for some ideal ``I ⊂ R`` in the original ring. 
"""
abstract type AbsLocalizedRingIdeal{RingType, RingElemType, MultSetType} <: Ideal{RingElemType} end

abstract type AbsLocalRingIdeal{RingType, RingElemType} <: AbsLocalizedRingIdeal{RingType, RingElemType, AbsComplementOfPrimeIdeal{RingType, RingElemType}} end

mutable struct MPolyLocalizedIdeal{BaseRingType, RingElemType, MultSetType} <: AbsLocalizedRingIdeal{MPolyRing{BaseRingType}, RingElemType, MultSetType}
  # to be filled with content
end

mutable struct MPolyLocalRingIdeal{BaseRingType, RingElemType} <: AbsLocalRingIdeal{MPolyRing{BaseRingType}, RingElemType}
  # to be filled with content
end

@Markdown.doc """
    LocalizedBiPolyArray{BaseRingType, RingElemType, MultSetType}

Main workhorse for binding of ideals in localizations of multivariate polynomial 
rings to the singular backend.
"""
mutable struct LocalizedBiPolyArray{BaseRingType, RingElemType, MultSetType}
  oscar_gens::Vector{AbstractAlgebra.Generic.Frac{RingElemType}}
  singular_gens::Singular.sideal
  oscar_ring::MPolyLocalizedRing{BaseRingType, RingElemType, MultSetType}
  singular_ring::Singular.PolyRing
 
  function LocalizedBiPolyArray(oscar_ring::MPolyLocalizedRing{BaseRingType, RingElemType, MultSetType}, oscar_gens::Vector{AbstractAlgebra.Generic.Frac{RingElemType}}) where {BaseRingType, RingElemType, MultSetType}
    BPA = new{BaseRingType, RingElemType, MultSetType}()
    # TODO: Add some sanity checks here
    BPA.oscar_ring = oscar_ring
    BPA.oscar_gens = oscar_gens
  end
end

function _singular_ring(oscar_ring::MPolyLocalizedRing{BaseRingType, RingElemType, MultSetType}; ord::Symbol = :degrevlex) where {BaseRingType, RingElemType, MultSetType}
  return Singular.PolynomialRing(Oscar.singular_ring(base_ring(original_ring(oscar_ring))), 
				 [string(a) for a = Nemo.symbols(original_ring(oscar_ring))], 
				 ordering = ord, 
				 cached = false)[1]
end

function singular_assure(I::LocalizedBiPolyArray{BaseRingType, RingElemType, MultSetType}) where {BaseRingType, RingElemType, MultSetType}
  if !isdefined(I.singular_ring)
    I.singular_ring = _singular_ring(oscar_ring)
    I.singular_ideal = Singular.Ideal(I.singular_ring, 
	[I.singular_ring(numerator(x)) for x in I.oscar_gens])
  end
end


