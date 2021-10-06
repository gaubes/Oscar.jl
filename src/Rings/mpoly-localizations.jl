export AbsMultCloSet, ComplementOfPrimeIdeal, ComplementOfMaximalIdeal, PowersOfElement
export is_element_of, localize_at

###############################################################################
# General framework for localizations of free polynomial rings                #
###############################################################################

@Markdown.doc """
    abstract type AbsMultCloSet{RingElemType} where RingElemType end

Abstract for a multiplicatively closed set in a commutative (Noetherian) ring 
R with elements of type `RingElemType`.
"""
abstract type AbsMultCloSet{RingElemType} end

@Markdown.doc """
    function is_element_of(f::RingElemType, S::AbsMultCloSet{RingElemType}) where RingElemType

Returns `true` if `f` belongs to the multiplicatively closed set `S` and false otherwise.
"""
function is_element_of(f::RingElemType, S::AbsMultCloSet{RingElemType}) where RingElemType
  error( "not implemented" )
end

@Markdown.doc """
    mutable struct ComplementOfPrimeIdeal{RingElemType} <: AbsMultCloSet{RingElemType}

The multiplicatively closed set S given as the complement S = R ∖ P
of a prime ideal P in a multivariate polynomial ring R over some base field.
"""
mutable struct ComplementOfPrimeIdeal{RingElemType} <: AbsMultCloSet{RingElemType}
end

function is_element_of(f::RingElemType, S::ComplementOfPrimeIdeal{RingElemType}) where RingElemType
  error( "not implemented" )
end

@Markdown.doc """
    mutable struct ComplementOfMaximalIdeal{RingElemType} <: AbsMultCloSet{RingElemType}  

The multiplicatively closed set S given as the complement S = R \\ 𝔪 
of a maximal ideal 𝔪 = ⟨x₁-a₁,…,xₙ-aₙ⟩ where R = 𝕜[x₁,…,xₙ] is a free polynomial 
ring over a base field 𝕜.

**Note:** This is not for *arbitrary* maximal ideals, but *only* for those of the form above, 
i.e. those which stay maximal when passing to an algebraic closure of the base field 𝕜.
"""
mutable struct ComplementOfMaximalIdeal{RingElemType} <: AbsMultCloSet{RingElemType}  
end

function is_element_of(f::RingElemType, S::ComplementOfMaximalIdeal{RingElemType}) where RingElemType
  error( "not implemented" )
end

@Markdown.doc """
    mutable struct PowersOfElement{RingElemType} <: AbsMultCloSet{RingElemType}

The multiplicatively closed set S of a free polynomial ring R = 𝕜[x₁,…,xₙ] given by the 
powers S = { fᵏ : k ∈ ℤ } for some polynomial f ∈ R.
"""
mutable struct PowersOfElement{RingElemType} <: AbsMultCloSet{RingElemType}
end

function is_element_of(f::RingElemType, S::PowersOfElement{RingElemType}) where RingElemType
  error( "not implemented" )
end


@Markdown.doc """
    function localize_at(S::AbsMultCloSet{RingElemType}) where RingElemType

Constructs the localization of the ring R ⊃ `S` at the multiplicatively closed 
subset `S` and returns this localized ring R[S⁻¹].

**Note:** the implementation is based on the general framework of fractions provided 
by `AbstractAlgebra.Generic.Frac` by simply limiting the set of admissible denominators.
"""
function localize_at( S::AbsMultCloSet )
  error( "not implemented" )
end

function localize_at( S::ComplementOfPrimeIdeal )
  error("this is supposed to use the graal alogithm. Not implemented, yet")
end

function localize_at( S::ComplementOfMaximalIdeal )
  error( "not implemented" )
end

function localize_at(S::PowersOfElement)
  error( "not implemented" )
end

