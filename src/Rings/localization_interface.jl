export AbsMultSet, AbsPowersOfElement, AbsComplementOfPrimeIdeal

export AbsLocalizedRing
export original_ring, inverted_set
export reduce_fraction
export localize_at

export AbsLocalizedRingElem
export numerator, denominator, parent

export AbsLocalizedIdeal
export ideal

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

### required getter functions
@Markdown.doc """
    ambient_ring(S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}

Returns the ambient ring `R` for a multiplicatively closed set `S ⊂ R`.
"""
function ambient_ring(S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}
  error("method `ambient_ring` not implemented for multiplicatively closed sets of type $(typeof(S))")
end

### required functionality
@Markdown.doc """
    in(f::RingElemType, S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}

Returns `true` if `f` belongs to `S`; `false` otherwise.

**Note:** If this routine is not implemented, the function call will default to the 
execution of an error message. 
"""
function Base.in(f::RingElemType, S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}
  error("method not implemented for multiplicatively closed sets of type $(typeof(S))")
end


#################################################################################
# Multiplicatively closed sets given by powers of one element                   #
#################################################################################

@Markdown.doc """
    AbsPowersOfElement{RingType, RingElemType} <: AbsMultSet{RingType, RingElemType}

Abstract type for the multiplicatively closed set ``S = { fᵏ : f ∈ R, k ∈ ℕ₀ } ⊂ R`` 
for some element ``f`` of type `RingElemType` in a ring ``R`` of type `RingType`.
"""
abstract type AbsPowersOfElement{RingType, RingElemType} <: AbsMultSet{RingType, RingElemType} end

### suggested, optional functionality
@Markdown.doc """
    denominators(P::AbsPowersOfElement{RingType, RingElemType}) where {RingType, RingElemType}

Returns a list of factors ``(a₁,…,aᵣ)`` of the element ``f = a₁⋅…⋅aᵣ`` defining ``P = { fᵏ : f ∈ R, k ∈ ℕ₀ }``.
"""
function denominators(P::AbsPowersOfElement{RingType, RingElemType}) where {RingType, RingElemType}
  error("method `generator(P::AbsPowersOfElement{RingType, RingElemType})` not implemented for `P` of type $(typeof(P))")
end


#################################################################################
# Multiplicatively closed sets given by complements of prime ideals             #
#################################################################################

@Markdown.doc """
    AbsComplementOfPrimeIdeal{RingType, IdealType, RingElemType}

Abstract type for the multiplicatively closed sets S = R \\ P for prime ideals P in a commutative ring R. 
This is comprises both the complement of maximal ideals, as well as arbitrary prime ideals. 
In general, one expects different algorithmic backends for each one of the two cases. This will lead to a 
distinction of the associated concrete types.
"""
abstract type AbsComplementOfPrimeIdeal{RingType, IdealType, RingElemType} <:AbsMultSet{RingType, RingElemType} end


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

### required getter functions
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

### required functionality
@Markdown.doc """
    localize_at(S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}

Returns the localization of the `ambient_ring` of `S` at `S` itself.
"""
function localize_at(S::AbsMultSet{RingType, RingElemType}) where {RingType, RingElemType}
  error("localizations at multiplicatively closed sets of type $(typeof(S)) are not implemented")
end

@Markdown.doc """
    (W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(f::AbstractAlgebra.Generic.Frac{RingElemType}) where {RingType, RingElemType, MultSetType} 

Converts a fraction f = a//b to an element of the localized ring W.
"""
function (W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(f::AbstractAlgebra.Generic.Frac{RingElemType}) where {RingType, RingElemType, MultSetType} 
  error("conversion for fractions to elements of type $(typeof(W)) is not implemented")
end

### required conversions
@Markdown.doc """
    (W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(a::RingElemType) where {RingType, RingElemType, MultSetType} 

Converts an element `a` to an element of `W`.
"""
function (W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(a::RingElemType) where {RingType, RingElemType, MultSetType} 
  error("conversion of elements of type $(RingElemType) to elements of $(typeof(W)) is not implemented")
end

@Markdown.doc """
    (W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(a::RingElemType, b::RingElemType) where {RingType, RingElemType, MultSetType} 

Converts a pair `(a, b)` to a fraction `a/b` in `W`.
"""
function (W::AbsLocalizedRing{RingType, RingElemType, MultSetType})(a::RingElemType, b::RingElemType) where {RingType, RingElemType, MultSetType} 
  error("conversion of pairs `(a, b)` of elements of type $(RingElemType) to fractions `a/b` in a ring of type $(typeof(W)) is not implemented")
end


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

### required getter functions 
@Markdown.doc """
    numerator(f::T) where {T<:AbsLocalizedRingElem}

Returns the numerator of `f`.
"""
function numerator(f::T) where {T<:AbsLocalizedRingElem}
  error("`numerator` is not implemented for elements of type $(typeof(f))")
end

@Markdown.doc """
    denominator(f::T) where {T<:AbsLocalizedRingElem}

Returns the denominator of `f`.
"""
function denominator(f::T) where {T<:AbsLocalizedRingElem}
  error("`denominator` is not implemented for elements of type $(typeof(f))")
end

@Markdown.doc """
    parent(f::T) where {T<:AbsLocalizedRingElem}

Returns the parent ring R[S⁻¹] of `f`.
"""
function parent(f::T) where {T<:AbsLocalizedRingElem}
  error("`parent` is not implemented for the type $(typeof(f))")
end

### default functionality for printing
function Base.show(io::IO, f::T) where {T<:AbsLocalizedRingElem}
  print(io, "$(numerator(f))//$(denominator(f))")
end


########################################################################
# Arithmetic; a dumb catchall implmentation, NOT performant!           #
########################################################################

@Markdown.doc """
    function reduce_fraction(a::T) where {T<:AbsLocalizedRingElem}

Reduce the fraction a = p/q. **Warning**: The catchall-implementation does nothing!
"""
function reduce_fraction(a::T) where {T<:AbsLocalizedRingElem}
  return a
end

function +(a::T, b::T) where {T<:AbsLocalizedRingElem}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  if denominator(a) == denominator(b) 
    return reduce_fraction((parent(a))(numerator(a) + numerator(b), denominator(a)))
  end
  return reduce_fraction((parent(a))(numerator(a)*denominator(b) + numerator(b)*denominator(a), denominator(a)*denominator(b)))
end

function -(a::T, b::T) where {T<:AbsLocalizedRingElem}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  if denominator(a) == denominator(b) 
    return reduce_fraction((parent(a))(numerator(a) - numerator(b), denominator(a)))
  end
  return reduce_fraction((parent(a))(numerator(a)*denominator(b) - numerator(b)*denominator(a), denominator(a)*denominator(b)))
end

function *(a::T, b::T) where {T<:AbsLocalizedRingElem}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  return reduce_fraction((parent(a))(numerator(a)*numerator(b), denominator(a)*denominator(b)))
end

function Base.:(//)(a::T, b::T) where {T<:AbsLocalizedRingElem}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  numerator(b) in inverted_set(parent(b)) || error("the second argument is not a unit in this local ring")
  return reduce_fraction((parent(a))(numerator(a)*denominator(b), numerator(b)*denominator(a)))
end

function ==(a::T, b::T) where {T<:AbsLocalizedRingElem}
  parent(a) == parent(b) || error("the arguments do not have the same parent ring")
  return numerator(a)*denominator(b) == numerator(b)*denominator(a)
end

function ^(a::T, i::Oscar.IntegerUnion) where {T<:AbsLocalizedRingElem}
  return parent(a)(numerator(a)^i, denominator(a)^i)
end


############################################################################
# Finitely generated ideals in localized rings                             #
############################################################################

@Markdown.doc """
    AbsLocalizedIdeal{RingType, RingElemType, MultSetType} <: Ideal{RingElemType}

Abstract type for ideals ``IS⁻¹ ⊂ R[S⁻¹]`` in localized rings. In fact, every 
such ideal is of this form for some ideal ``I ⊂ R`` in the original ring. 
"""
abstract type AbsLocalizedRingIdeal{RingType, RingElemType, MultSetType} <: Ideal{RingElemType} end


############################################################################
# Finitely generated modules over localized rings                          #
############################################################################
