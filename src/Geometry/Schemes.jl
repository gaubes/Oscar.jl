
import AbstractAlgebra.Ring, Oscar.AlgHom, Oscar.compose, AbstractAlgebra.Generic.Frac
import Base: ∘
import Oscar: base_ring

export AffineScheme, PrincipalSubScheme,Spec, SpecPrincipalOpen, affine_space
export AffSchMorphism, base_ring,ambient_ring,defining_ideal, pullback
export localize

abstract type Scheme end
abstract type AffineScheme{S <: Ring, T <: MPolyRing, U <: MPolyElem} <: Scheme end
abstract type SchemeMorphism end

####################################################################################
# The classical affine scheme, explicitly given as the quotient 
# ring of a polynomial algebra.
mutable struct Spec{S,T,U} <: AffineScheme{S,T,U}
  # the basic fields 
  k::S			# the base ring (usually a field) of definition for the scheme
  R::T		  	# the ambient polynomial ring to model this affine scheme
  I::MPolyIdeal{U}	# The ideal in R defining the scheme

  function Spec(k::S, R::T, I::MPolyIdeal{U} ) where{
			S <: Ring, T <:MPolyRing , U <: MPolyElem}
    if k != coefficient_ring(R)
      error( "Base ring of the affine scheme does not coincide with the base ring of the associated algebra." )
    end
    # TODO: Implement further plausibility checks to be performed at runtime.
    return new{S, T, U}(k, R, I )
  end
end

###################################################################################
# Getter functions
#
# No caching is needed in this case, since all these variables need to be assigned 
# at instantiation
#
function base_ring(A::Spec)
  return A.k
end

function ambient_ring(A::Spec)
  return A.R
end

function defining_ideal(A::Spec)
  return A.I
end

##################################################################################
# Affine scheme that arises as the localization of a Spec at a specific 
# element 'denom' of the coordinate ring, i.e. a principal open subset. 
#
# These are implemented in a recursive fashion meaning there is a parent 
# which is either a classical Spec or another principal open subset, from 
# which this instance is derived by inverting an element 'denom'. 
#
# Such parent structures form a tree which necessarily has a Spec at its 
# root. Currently, the implementation only allows localizations at elements 
# denom from the coordinate ring of this root. 
#
mutable struct SpecPrincipalOpen{S,T,U} <: AffineScheme{S,T,U}
  parent::AffineScheme{S,T,U}
  denom::U  # element of the "ambient" polynomial ring of the root

  # Fields for caching. These provide the data as a Spec for this 
  # affine scheme
  k::S  
  R::T
  I::MPolyIdeal{U} 
  pullbackFromParent::AlgHom
  pullbackFromRoot::AlgHom
  u::U # The inverse of denom in R
  
  function SpecPrincipalOpen(parent::Union{Spec{S,T,U},SpecPrincipalOpen{S,T,U}}, denom::U) where {S <: Ring, T<:MPolyRing, U<:MPolyElem}
    x = new{S,T,U}() 
    x.parent = parent
    # TODO: Implement a plausibility check: Does denom belong to the coordinate ring of the root?
    x.denom = denom
    return x 
  end 
end

###############################################################################
# Getter functions

function PrincipalSubScheme(parent, denom)
  return SpecPrincipalOpen(parent, denom)
end

function localize(parent, denom)
  return SpecPrincipalOpen(parent, denom)
end

function parent(S::AffineScheme)
  return S
end

function parent(D::SpecPrincipalOpen)
  return D.parent
end 

function root(D::SpecPrincipalOpen)
  return parent(parent(D))
end

root(A::Spec) = A

function base_ring(D::SpecPrincipalOpen)
  if isdefined(D,:k)
    return D.k
  end
  k = base_ring(root(D))
  D.k = k
  return k
end

# Collect the denominators from this localization up to the root. 

function denoms(D::SpecPrincipalOpen{S,T,U}) where {S <: Ring, T <: MPolyRing, U<:MPolyElem}
  result = U[]
  P = D
  while typeof(P) <: SpecPrincipalOpen
    push!(result, P.denom)
    P = parent(P)
  end
  return result
end 

# Set up an ambient ring for this localization. 
#
# The current implementation adds a variable 'denom$i' for 
# every element whose inverse has been added to the 
# coordinate ring of the root. Again, the implementation is 
# recursive, but flattened in the sense that all variables
# are on the same level over the base ring.
 
function ambient_ring(D::SpecPrincipalOpen)
  if isdefined( D, :R )
    return D.R
  end
  #names = ["denom$i" for i in 1:length(denoms(D))]
  n = length( denoms( D )) + 1
  R = ambient_ring(parent(D))
  names = vcat( names, String.( symbols( R)) )
  S, ϕ, u = add_variables( R, ["denom$n"] )
  D.R = S
  D.u = u
  D.pullbackFromParent = ϕ
  D.pullbackFromRoot = compose( ϕ, pullbackFromRoot( parent( D )))
  return S
end 

function pullback_from_root( D::SpecPrincipalOpen )
  if isdef( D, :pullbackFromRoot )
    return D.pullback_from_root
  end
  ambient_ring( D ) # This also stores the homomorphism
  return D.pullbackFromRoot
end

function pullback_from_parent( D::SpecPrincipalOpen )
  if isdef( D, :pullbackFromParent )
    return D.pullback_from_parent
  end
  ambient_ring( D ) # This also stores the homomorphism
  return D.pullbackFromParent
end

#function get_ring_hom(D::SpecPrincipalOpen)
#  R = ambient_ring(root(D))
#  S = ambient_ring(D)
#  d = length(denoms(D))
#  n = length(gens(R))
#  return AlgebraHomomorphism(R,S, gens(S)[d+1:n+d])
#end

function defining_ideal(D::SpecPrincipalOpen)
  ϕ = pullback_from_parent(D)
  I = defining_ideal(parent(D))
  R = ambient_ring(D)
  J = ideal( J, [ ϕ(f) for f in gens( I ) ])
  J = J + ideal( J, [ 1-u*denom ] )
  return ( J )
end


# outer constructors
function Spec( k::S, R::T ) where{S <: Ring, T <:MPolyRing}
  I = ideal(R, zero(R))
  return Spec(k, R, I )
end

@doc Markdown.doc"""
    Spec(R::MPolyRing) -> Spec

Return the affine scheme corresponding to the ring $R/(0)$.
"""
function Spec( R::T ) where{T <: MPolyRing}
  I = ideal(R, zero(R))
  k = coefficient_ring( R )
  return Spec(k, R, I )
end

function Spec( R::T, I::MPolyIdeal{U} ) where{ T <: MPolyRing, U <: MPolyElem }
  k = coefficient_ring(R)
  return Spec(k, R, I )
end

# Construct affine n-space over the ring k.
function affine_space( k::Ring, n::Int, name::String="x" )
  R, x = PolynomialRing( k, name => (1:n))
  return Spec( R )
end

function Base.show( io::Base.IO, X::AffineScheme )
  Base.print( io, "Affine scheme over " )
  Base.print( io, base_ring(X) )
  Base.print( io, "\n" ) 
  Base.print( io, "given as the quotient of the polynomial ring \n" )
  Base.print( io, ambient_ring(X) )
  Base.print( io, "\nby the ideal \n" )
  Base.print( io, defining_ideal(X) )
end


function Base.show( io::Base.IO, X::SpecPrincipalOpen)
  Base.print( io, "Principal open subscheme of \n" )
  Base.print( io, root(X) )
  Base.print( io, "\n" )
  Base.print( io, "defined by\n" )
  Base.print( io,  denoms(X))
end

############################################################################
# Morphisms of affine schemes.
#
# These need to take into account both the Specs and their localizations. 
# The information on the morphism is stored either as an explicit 
# homomorphism of polynomial rings for the coordinate rings of the 
# associated specs, or as a list of rational functions, the images of 
# the variables. 

mutable struct AffSchMorphism{S,Tdom, Udom, Tcod, Ucod}
  domain::AffineScheme{S, Tdom, Udom}
  codomain::AffineScheme{S, Tcod, Ucod}
  pullback::AlgHom
  imgs_frac::Vector{Frac{Ucod}}

  function AffSchMorphism( domain::AffineScheme{S,Td,Ud},
                           codomain::AffineScheme{S,Tc,Uc}, pullback::AlgHom
                           ) where {S,Td,Ud,Tc,Uc}

   if base_ring(domain) != base_ring(codomain)
      error( "the base rings of the domain and the codomain do not coincide!" )
    end
    k = base_ring(domain)
    #if domain(pullback) != ambient_ring(R) || codomain(pullback) != domain.R
      #error( "the domain and codomain of the given ring homomorphism is not compatible with the affine schemes." )
    #end
    x = new{S,Td,Ud,Tc,Uc}()
    x.domain = domain
    x.codomain = codomain
    x.pullback = pullback
    return x

  end

  function AffSchMorphism( domain::AffineScheme{S,Td,Ud},
                           codomain::AffineScheme{S,Tc,Uc}, imgs_frac::Frac{Uc}
                           ) where {S,Td,Ud,Tc,Uc}
    if base_ring(domain) != base_ring(codomain)
      error( "the base rings of the domain and the codomain do not coincide!" )
    end
    k = base_ring(domain)
    #if domain(pullback) != codomain.R || codomain(pullback) != domain.R
      #error( "the domain and codomain of the given ring homomorphism is not compatible with the affine schemes." )
    #end
    x = new{S,Td,Ud,Tc,Uc}()
    x.domain = domain
    x.codomain = codomain
    x.imgs_frac = imgs_frac
    return x
  end
end

domain(f::AffSchMorphism) = f.domain
codomain(f::AffSchMorphism) = f.codomain

# Construct the pullback on the level of coordinate rings 
# from the fractional representation 
function pullback(f::AffSchMorphism)
  if isdefined(f, :pullback)
    return f.pullback
  end
  if !isdefined(f, :imgs_frac )
    error( "Neither the fractional representation, nor the pullback is defined for this morphism." )
  end
  R = base_ring(codomain(f))
  S = base_ring(domain(f))
  # TODO reconstruct the ring homomorphism R -> S from the fractional representation.
end

# Construct the fractional representation of the morphism 
# from the explicit algebra homomorphism. 
function imgs_frac(f::AffSchMorphism)
  # first check if this variable is already cached
  if isdefined(f, Symbol("imgs_frac"))
    return f.imgs_frac
  end
  # if both forms of the morphisms are not to be found, it is not defined at all.
  if !isdefined( f, :pullback )
    error( "Neither the fractional representation, nor the pullback is defined for this morphism." )
  end

  # start reconstructing the fraction representation from 
  # the explicit ring homomorphism
  ϕ = pullback(f)
  # Set up the codomain of a lift of the pullback ϕ to the ambient ring of 
  # the domain of f. This is a localization of a *free* polynomial ring, 
  # considered as a subalgebra of the fraction field. 
  P = ambient_ring(root(domain(f)))
  F = FractionField(P)
  den = denoms(domain(f))
  d = length(den)
  n = length(gens(P))
  # prepare a list of the images of the generators 
  fracs = [ F(x) for x in gens(P) ]
  fracs = vcat( fracs, [ 1//g for g in den ])
  R = ambient_ring(root(codomain(f)))
  imgs_frac = elem_type(F)[]
  for g in gens(R)
    h = compose( ϕ, pullback_from_root(codomain(f)) )
    push!(imgs_frac,evaluate(h(g), fracs))
  end
  f.imgs_frac = imgs_frac
  return imgs_frac
end


# Todo adapt the code below to the new data structure

#=
@doc Markdown.doc"""
    struct Glueing( domain::AffineScheme, codomain::AffineScheme, pullback::AlgHom )

    Maintains the data for the glueing of two affine varieties.
    In practice, domain and codomain will be distinguished open subsets
    of different affine algebras and then pullback will specify
    a concrete isomorphism between them.

    Compared to AffSchMorphism, this structure has an additional field
    in which the inverse morphism can be stored.
"""
mutable struct Isomorphism{
    Sdom <: Ring, Tdom <: MPolyRing, Udom <: MPolyElem,
    Scod <: Ring, Tcod <: MPolyRing, Ucod <: MPolyElem
  } <: SchemeMorphism
  domain::Spec{ Sdom, Tdom, Udom }
  codomain::Spec{ Scod, Tcod, Ucod }
  pullback::AlgHom
  inverse::AlgHom

  function Isomorphism( domain::AffineScheme, codomain::AffineScheme, pullback::AlgHom, inverse::AlgHom )
    if coefficient_ring( domain.R ) != coefficient_ring( codomain.R )
      error( "the base rings of the domain and the codomain do not coincide!" )
    end
    #if domain(pullback) != codomain.R || codomain(pullback) != domain.R
      #error( "the domain and codomain of the given ring homomorphism is not compatible with the affine schemes." )
    #end
    return new{
      typeof(coefficient_ring(domain.R)), typeof(domain.R), elem_type(domain.R),
      typeof(coefficient_ring(codomain.R)), typeof(codomain.R), elem_type(codomain.R)
    }( domain, codomain, pullback, inverse )
  end

  function Isomorphism( domain::AffineScheme, codomain::AffineScheme, pullback::AlgHom )
    if coefficient_ring( domain.R ) != coefficient_ring( codomain.R )
      error( "the base rings of the domain and the codomain do not coincide!" )
    end
    inv_list = [ preimage( pullback, g )[1] for g in gens( codomain( pullback ))]
    inverse = AlgebraHomomorphism( pullback.codomain, pullback.domain, inv_list )
    return new{
      typeof(coefficient_ring(domain.R)), typeof(domain.R), elem_type(domain.R),
      typeof(coefficient_ring(codomain.R)), typeof(codomain.R), elem_type(codomain.R)
    }( domain, codomain, pullback, inverse )
  end
end

# The inclusion of open subsets. 
# Note that this struct can only instantiate open inclusions which 
# are determined by an explicit ring homomorphism. Not all open 
# inclusions (not even of affine schemes) can be described this way!
mutable struct OpenInclusion{
    Sdom <: Ring, Tdom <: MPolyRing, Udom <: MPolyElem, 
    Scod <: Ring, Tcod <: MPolyRing, Ucod <: MPolyElem 
  } <: SchemeMorphism
  domain::Spec{ Sdom, Tdom, Udom }
  codomain::Spec{ Scod, Tcod, Ucod }
  pullback::AlgHom
end

function Oscar.compose( phi::AffSchMorphism, psi::AffSchMorphism )
  if psi.codomain != phi.domain 
    error( "Morphisms of schemes can not be composed." )
  end
  return AffSchMorphism( phi.domain, psi.codomain, compose( psi.pullback, phi.pullback ))
end

function Oscar.compose( phi::Isomorphism, psi::AffSchMorphism )
  if psi.codomain != phi.domain 
    error( "Morphisms of schemes can not be composed." )
  end
  return AffSchMorphism( phi.domain, psi.codomain, compose( psi.pullback, phi.pullback ))
end

function Oscar.compose( phi::AffSchMorphism, psi::Isomorphism )
  if psi.codomain != phi.domain 
    error( "Morphisms of schemes can not be composed." )
  end
  return AffSchMorphism( phi.domain, psi.codomain, compose( psi.pullback, phi.pullback ))
end

function Oscar.compose( phi::Isomorphism, psi::Isomorphism )
  if psi.codomain != phi.domain 
    error( "Morphisms of schemes can not be composed." )
  end
  return Isomorphism( phi.domain, psi.codomain, 
		 compose( psi.pullback, phi.pullback ),
		 compose( phi.inverse, psi.inverse) )
end

function ∘( phi::SchemeMorphism, psi::SchemeMorphism )
  return compose( phi, psi )
end

# A struct for glueing of two affine schemes X = Spec A and Y = Spec B along a common 
# principal open subset U. Then U is given as both, Spec A_f and Spec B_g for 
# elements f ∈  A and g ∈  B, and there is an isomorphism ϕ : A_f →  B_g. The 
# Glueing maintains this information, together with the two inclusions of 
# open sets i : U ↪  X and j : U ↪  Y given by A →  A_f, and B →  B_f, respectively.
mutable struct Glueing
  X::AffineScheme
  Y::AffineScheme
  i::SchemeMorphism
  j::SchemeMorphism
  ϕ::Isomorphism

end

# A struct maintaining the information for a covering 
# of a scheme. A scheme itself can have multiple coverings 
# with a partial order given by refinement. Any object relying 
# on a covering such as, for instance, a vector bundle given by 
# transition functions, can refer to such a covering. 
#
# TODO: Simon argued that such a struct should not be mutable, 
# because the objects depending on it can not keep track 
# of such changes. Technically, however, that means all data 
# for the covering has to be computed before the actual instantiation 
# of the covering. That will involve a lot of temporary variables 
# resembling the final struct and seems a bit tedious. Therefore, 
# I make it mutable for now. 
mutable struct Covering 
  charts::Vector{AffineScheme} 
  glueings::Vector{Glueing}
end

mutable struct CoveredScheme <: Scheme
  coverings::Vector{Covering}
end

abstract type ChowCycle end

# This is the data structure for a cycle in an affine 
# scheme given by a list of coefficients and prime ideals
# TODO: Parametrize by types.
mutable struct AffineCycle{ CoefficientType <: Ring } <: ChowCycle
  parent::AffineScheme
  summands::Tuple{CoefficientType, AbstractAlgebra.Ideal}
end

=#
