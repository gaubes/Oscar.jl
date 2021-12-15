import AbstractAlgebra.Ring
import Base: intersect

export Spec, OO
export ⊂

export is_open_embedding, is_closed_embedding, hypersurface_complement, subscheme, name_of, set_name!
export closure, product

export SpecMor
export pullback, domain, codomain, preimage, restrict, graph

@Markdown.doc """
Scheme{ BaseRingType <: Ring }

A scheme over a ring ``k`` of type `BaseRingType`.
"""
abstract type Scheme{BaseRingType<:Ring, BaseRingElemType<:RingElement} end

struct EmptyScheme{BaseRingType, BaseRingElemType}<:Scheme{BaseRingType, BaseRingElemType} 
  k::BaseRingType
  function EmptyScheme(k::BaseRingType) where {BaseRingType<:Ring}
    return new{BaseRingType, elem_type(k)}(k)
  end
end

@doc Markdown.doc"""
AffineScheme{BaseRingType<:Ring, BaseRingElemType<:RingElement, RingType<:MPolyRing, RingElemType<:MPolyElem} <: Scheme{BaseRingType} 

An affine scheme over a base ring ``k`` of type `BaseRingType` and
elements of type `BaseRingElemType`, given by a ring ``R/I`` with ``R``
a polynomial ring of type `RingType` and elements of type `RingElemType`.
"""
abstract type AffineScheme{BaseRingType<:Ring, BaseRingElemType<:RingElement, RingType<:MPolyRing, RingElemType<:MPolyElem, MultSetType<:AbsMPolyMultSet} <: Scheme{BaseRingType, BaseRingElemType} end

@doc Markdown.doc"""
Spec{BRT, BRET, RT, RET} <: AffineScheme{BRT, BRET, RT, RET}

An affine scheme ``X = Spec (R/I)[S⁻¹]`` with ``R = k[x₁,…,xₙ]`` a free 
polynomial algebra of type `RT` over a base ring ``k`` of type 
`BRT` and ``I ⊂ R`` a finitely generated ideal 
with elements of type `RET`.
"""
mutable struct Spec{BRT, BRET, RT, RET, MST} <: AffineScheme{BRT, BRET, RT, RET, MST}
  # the basic fields 
  OO::MPolyQuoLocalizedRing{BRT, BRET, RT, RET, MST}
  # fields for caching
  name::String # the name of this scheme for printing

  function Spec(OO::MPolyQuoLocalizedRing{BRT, BRET, RT, RET, MST}) where {BRT, BRET, RT, RET, MST}
    return new{BRT, BRET, RT, RET, MST}(OO)
  end
end


### Getter functions

OO(X::Spec) = X.OO

function name_of(X::Spec) 
  if isdefined(X, :name)
    return X.name
  end
  return "unnamed affine variety"
end

function set_name!(X::Spec, name::String) 
  X.name = name
end

function Base.show(io::IO, X::Spec) 
  if isdefined(X, :name)
    print(io, name_of(X))
    return
  end
  print(io, "Spec of $(OO(X))")
end

### Copy constructor
Spec(X::Spec) = Spec(OO(X))

### internal copy routines
Base.deepcopy_internal(X::Spec, dict::IdDict) = Spec(deepcopy_internal(OO(X), dict))

### additional constructors 
Spec(R::MPolyRing) = Spec(MPolyQuoLocalizedRing(R))

Spec(Q::MPolyQuo) = Spec(MPolyQuoLocalizedRing(Q))

Spec(W::MPolyLocalizedRing) = Spec(MPolyQuoLocalizedRing(W))

Spec(R::MPolyRing, I::MPolyIdeal, U::AbsMPolyMultSet) = Spec(MPolyQuoLocalizedRing(R, I, U))

### closed subschemes defined by ideals
function subscheme(X::Spec{BRT, BRET, RT, RET, MST}, I::MPolyLocalizedIdeal{BRT, BRET, RT, RET, MST}) where {BRT, BRET, RT, RET, MST}
  localized_ring(OO(X)) == base_ring(I) || error("ideal does not live in the correct ring")
  return Spec(quo(localized_ring(OO(X)), I + localized_modulus(OO(X))))
end

function subscheme(X::Spec{BRT, BRET, RT, RET, MST}, I::MPolyIdeal{RET}) where {BRT, BRET, RT, RET, MST}
  base_ring(OO(X)) == base_ring(I) || error("ideal does not live in the correct ring")
  return Spec(quo(OO(X), I))
end
  
function subscheme(X::Spec{BRT, BRET, RT, RET, MST}, f::RET) where {BRT, BRET, RT, RET, MST}
  R = base_ring(OO(X))
  I = ideal(R, f)
  return subscheme(X, I)
end

function subscheme(X::Spec{BRT, BRET, RT, RET, MST}, f::Vector{RET}) where {BRT, BRET, RT, RET, MST}
  R = base_ring(OO(X))
  I = ideal(R, f)
  return subscheme(X, I)
end

function subscheme(X::Spec{BRT, BRET, RT, RET, MST}, f::BRET) where {BRT, BRET, RT, RET, MST}
  R = base_ring(OO(X))
  I = ideal(R, R(f))
  return subscheme(X, I)
end

function subscheme(X::Spec{BRT, BRET, RT, RET, MST}, f::Vector{BRET}) where {BRT, BRET, RT, RET, MST}
  R = base_ring(OO(X))
  I = ideal(R, R.(f))
  return subscheme(X, I)
end

### open subschemes defined by complements of hypersurfaces
function hypersurface_complement(X::Spec{BRT, BRET, RT, RET, MST}, f::RET) where {BRT, BRET, RT, RET, MST<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  R = base_ring(OO(X))
  parent(f) == R || error("the element does not belong to the correct ring")
  return Spec(Localization(OO(X), MPolyPowersOfElement(f)))
end

function hypersurface_complement(
    X::Spec{BRT, BRET, RT, RET, MST}, 
    f::MPolyLocalizedRingElem{BRT, BRET, RT, RET, MST}
  ) where {BRT, BRET, RT, RET, MST<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  return Spec(Localization(OO(X), MPolyPowersOfElement(numerator(f))))
end

function hypersurface_complement(
    X::Spec{BRT, BRET, RT, RET, MST}, 
    f::MPolyQuoLocalizedRingElem{BRT, BRET, RT, RET, MST}
  ) where {BRT, BRET, RT, RET, MST<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  parent(f) == OO(X) || error("the element does not belong to the correct ring")
  return Spec(Localization(OO(X), MPolyPowersOfElement(lifted_numerator(f))))
end


### testing containment
⊂(
  X::Scheme{BRT, BRET}, 
  Y::Scheme{BRT, BRET}
 ) where {BRT, BRET} = issubset(X, Y)

issubset(X::EmptyScheme{BRT, BRET}, Y::Scheme{BRT, BRET}) where {BRT, BRET} = true

function issubset(
    X::Spec{BRT, BRET, RT, RET, MST1}, 
    Y::Spec{BRT, BRET, RT, RET, MST2}
  ) where {BRT, BRET, RT, RET, MST1<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST2<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  R = base_ring(OO(X))
  R == base_ring(OO(Y)) || return false
  UX = inverted_set(OO(X))
  UY = inverted_set(OO(Y))
  if !issubset(UY, UX) 
    # check whether the inverted elements in Y are units anyway
    for a in denominators(UY)
      isunit(OO(X)(a)) || return false
    end
  end
  J = localized_ring(OO(X))(modulus(OO(Y)))
  return issubset(J, localized_modulus(OO(X)))
end

function ==(
    X::Spec{BRT, BRET, RT, RET, MST1}, 
    Y::Spec{BRT, BRET, RT, RET, MST2}
  ) where {BRT, BRET, RT, RET, MST1<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST2<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  return issubset(X, Y) && issubset(Y, X)
end

function is_open_embedding(
    X::Spec{BRT, BRET, RT, RET, MST1}, 
    Y::Spec{BRT, BRET, RT, RET, MST2}
  ) where {BRT, BRET, RT, RET, MST1<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST2<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  R = base_ring(OO(X))
  R == base_ring(OO(Y)) || return false
  UX = inverted_set(OO(X))
  UY = inverted_set(OO(Y))
  issubset(UY, UX) || return false
  J = localized_ring(OO(X))(modulus(OO(Y)))
  return localized_modulus(OO(X)) == J 
end

function is_closed_embedding(
    X::Spec{BRT, BRET, RT, RET, MST1}, 
    Y::Spec{BRT, BRET, RT, RET, MST2}
  ) where {BRT, BRET, RT, RET, MST1<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST2<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  R = base_ring(OO(X))
  R == base_ring(OO(Y)) || return false
  inverted_set(OO(X)) == inverted_set(OO(Y)) || return false
  J = localized_ring(OO(X))(modulus(OO(Y)))
  return issubset(J, localized_modulus(OO(X)))
end

### set operations

intersect(E::EmptyScheme{BRT, BRET}, X::Scheme{BRT, BRET}) where {BRT, BRET} = E
intersect(X::Scheme{BRT, BRET}, E::EmptyScheme{BRT, BRET}) where {BRT, BRET} = E
intersect(X::EmptyScheme{BRT, BRET}, E::EmptyScheme{BRT, BRET}) where {BRT, BRET} = E

function Base.intersect(
    X::Spec{BRT, BRET, RT, RET, MST1}, 
    Y::Spec{BRT, BRET, RT, RET, MST2}
  ) where {BRT, BRET, RT, RET, MST1<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST2<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  issubset(X, Y) && return X
  issubset(Y, X) && return Y
  UX = inverted_set(OO(X))
  UY = inverted_set(OO(Y))
  U = UX*UY
  IX = modulus(OO(X))
  IY = modulus(OO(Y))
  I = IX + IY
  R = base_ring(OO(X))
  L = MPolyQuoLocalizedRing(R, I, U)
  one(localized_ring(L)) in localized_modulus(L) && return EmptyScheme(coefficient_ring(R))
  return Spec(L)
end

### compute the closure of X in Y
function closure(
    X::Spec{BRT, BRET, RT, RET, MST1}, 
    Y::Spec{BRT, BRET, RT, RET, MST2}
  ) where {BRT, BRET, RT, RET, MST1<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST2<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  issubset(X, Y) || error("the first argument is not a subset of the second")
  is_closed_embedding(X, Y) && return X
  W = Localization(inverted_set(OO(X))*inverted_set(OO(Y)))
  I = ideal(W, W.(gens(modulus(OO(X)))))
  lbpa = groebner_basis(I) # takes care of the saturation
  R = base_ring(OO(Y))
  return Spec(MPolyQuoLocalizedRing(R, ideal(R, numerator.(oscar_gens(lbpa))), inverted_set(OO(Y))))
end



########################################################################
# Morphisms of affine schemes                                      #
########################################################################

mutable struct SpecMor{BRT, BRET, RT, RET, MST1, MST2}
  domain::Spec{BRT, BRET, RT, RET, MST1}
  codomain::Spec{BRT, BRET, RT, RET, MST2}
  pullback::MPolyQuoLocalizedRingHom{BRT, BRET, RT, RET, MST2, MST1}

  # fields used for caching
  inverse::SpecMor{BRT, BRET, RT, RET, MST2, MST1}

  function SpecMor(
      X::Spec{BRT, BRET, RT, RET, MST1},
      Y::Spec{BRT, BRET, RT, RET, MST2},
      pullback::MPolyQuoLocalizedRingHom{BRT, BRET, RT, RET, MST2, MST1}
    ) where {BRT, BRET, RT, RET, MST1, MST2}
    OO(X) == codomain(pullback) || error("the coordinate ring of the domain does not coincide with the codomain of the pullback")
    OO(Y) == domain(pullback) || error("the coordinate ring of the codomain does not coincide with the domain of the pullback")
    return new{BRT, BRET, RT, RET, MST1, MST2}(X, Y, pullback)
  end
end

### getter functions
pullback(phi::SpecMor) = phi.pullback
domain(phi::SpecMor) = phi.domain
codomain(phi::SpecMor) = phi.codomain

### additional constructors
function SpecMor(
      X::Spec{BRT, BRET, RT, RET, MST1},
      Y::Spec{BRT, BRET, RT, RET, MST2},
      f::Vector
  ) where {BRT, BRET, RT, RET, MST1, MST2}
  return SpecMor(X, Y, MPolyQuoLocalizedRingHom(OO(Y), OO(X), f))
end

function restrict(f::SpecMor{BRT, BRET, RT, RET, MST1, MST2}, 
    U::Spec{BRT, BRET, RT, RET, MST1},
    V::Spec{BRT, BRET, RT, RET, MST2}
  ) where {BRT, BRET, RT, RET, MST1, MST2}
  issubset(U, domain(f)) || error("second argument does not lay in the domain of the map")
  issubset(V, codomain(f)) || error("third argument does not lay in the codomain of the map")
  issubset(U, preimage(f, V)) || error("the image of the restriction is not contained in the restricted codomain")
  return SpecMor(U, V, images(pullback(f)))
end

function compose(f::SpecMorType, g::SpecMorType) where {SpecMorType<:SpecMor}
  codomain(f) == domain(g) || error("Morphisms can not be composed")
  return SpecMor(domain(f), codomain(g), compose(pullback(g), pullback(f)))
end

function ==(f::SpecMorType, g::SpecMorType) where {SpecMorType<:SpecMor}
  domain(f) == domain(g) || return false
  codomain(f) == codomain(g) || return false
  pullback(f) == pullback(g) || return false
  return true
end

### functionality
function preimage(
    phi::SpecMor{BRT, BRET, RT, RET, MST1, MST2}, 
    Z::Spec{BRT, BRET, RT, RET, MST3}
  ) where {BRT, BRET, RT, RET, MST1<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST2<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST3<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  X = domain(phi)
  Y = codomain(phi)
  issubset(Z, Y) || (Z = intersect(Y, Z))
  IZ = modulus(OO(Z))
  a = denominators(inverted_set(OO(Z)))
  R = base_ring(OO(X))
  f = pullback(phi)
  new_units = MPolyPowersOfElement(R, [lifted_numerator(f(d)) for d in a])
  new_gens = lifted_numerator.(f.(gens(IZ)))
  return Spec(MPolyQuoLocalizedRing(R, ideal(R, new_gens) + modulus(OO(X)), inverted_set(OO(X))*new_units))
end

function is_isomorphism(f::SpecMor{BRT, BRET, RT, RET, MST1, MST2}) where {BRT, BRET, RT, RET, MST1<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST2<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  is_isomorphism(pullback(f)) || return false
  f.inverse = SpecMor(codomain(f), domain(f), inverse(pullback(f)))
  return true
end

function inverse(f::SpecMor{BRT, BRET, RT, RET, MST1, MST2}) where {BRT, BRET, RT, RET, MST1<:MPolyPowersOfElement{BRT, BRET, RT, RET}, MST2<:MPolyPowersOfElement{BRT, BRET, RT, RET}}
  if isdefined(f, :inverse) 
    return f.inverse
  end
  is_isomorphism(f) || error("the given morphism is not an isomorphism")
  return f.inverse
end

function product(X::Spec{BRT, BRET, RT, RET, MST}, Y::Spec{BRT, BRET, RT, RET, MST}) where {BRT, BRET, RT, RET, MST<:MPolyPowersOfElement}
  K = OO(X)
  L = OO(Y) 
  V = localized_ring(K)
  W = localized_ring(L)
  R = base_ring(K)
  S = base_ring(L)
  k = base_ring(R)
  k == base_ring(S) || error("varieties are not defined over the same field")

  m = length(gens(R))
  n = length(gens(S))
  RS, z = PolynomialRing(k, vcat(symbols(R), symbols(S)))
  inc1 = AlgebraHomomorphism(R, RS, gens(RS)[1:m])
  inc2 = AlgebraHomomorphism(S, RS, gens(RS)[m+1:m+n])
  #pr1 = AlgebraHomomorphism(RS, R, vcat(gens(R), [zero(R) for i in 1:n]))
  #pr2 = AlgebraHomomorphism(RS, S, vcat([zero(S) for i in 1:m], gens(S)))
  IX = ideal(RS, inc1.(gens(modulus(OO(X)))))
  IY = ideal(RS, inc2.(gens(modulus(OO(Y)))))
  UX = MPolyPowersOfElement(RS, inc1.(denominators(inverted_set(OO(X)))))
  UY = MPolyPowersOfElement(RS, inc2.(denominators(inverted_set(OO(Y)))))
  XxY = Spec(RS, IX + IY, UX*UY)
  pr1 = SpecMor(XxY, X, gens(RS)[1:m])
  pr2 = SpecMor(XxY, Y, gens(RS)[m+1:m+n])
  return XxY, pr1, pr2
end

  
function graph(f::SpecMor{BRT, BRET, RT, RET, MST, MST}) where {BRT, BRET, RT, RET, MST<:MPolyPowersOfElement}
  X = domain(f)
  Y = codomain(f)
  XxY, prX, prY = product(X, Y)
  pb_X = pullback(prX)
  pb_Y = pullback(prY)
  pb_f = pullback(f)
  I = ideal(localized_ring(OO(XxY)), lift.(pb_X.(images(pb_f)) - pb_Y.(gens(OO(Y)))))
  G = subscheme(XxY, I)
  return G, restrict(prX, G, X), restrict(prY, G, Y)
end

function partition_of_unity(X::Spec{BRT, BRET, RT, RET, MST},
    f::Vector{RET}
  ) where {BRT, BRET, RT, RET, MST}
  error("not implemented")
end
