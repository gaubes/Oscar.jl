import AbstractAlgebra.Ring
import AbstractAlgebra.RingElement
import Oscar: radical_membership


module Misc
using Oscar
using Markdown
import AbstractAlgebra.Ring
import AbstractAlgebra.Generic.MatSpaceElem

export add_variables, divides_power, diag

####################################################################
#  
#  Miscellaneous routines used in the above 
#
####################################################################


function add_variables( R::T, new_vars::Vector{String} ) where{ T<:MPolyRing } 
  k = base_ring(R)
  old_vars = String.( symbols(R) )
  n = length( old_vars )
  vars = vcat( old_vars, new_vars )
  S, v = PolynomialRing( k, vars )
  phi = AlgebraHomomorphism( R, S, gens(S)[1:n] )
  y = v[n+1:length(v)]
  return S, phi, y
end

function add_variables( P::T, new_vars::Vector{String} ) where{ T<:MPolyQuo } 
  R = base_ring(P)
  k = base_ring(R)
  S, phi, y = add_variables( R, new_vars )
  I = ideal( S, [ phi(g) for g in gens(modulus(P)) ] )
  Q, projection = quo( S, I )
  phi_quot = AlgebraHomomorphism( P, Q, [projection(g) for g in phi.image] )
  return Q, phi_quot, [ projection(g) for g in y]
end

####################################################################
#
# Test whether some power of the polynomial f is contained in the 
# principal ideal generated by g
#
function Oscar.radical_membership( f::U, g::U ) where{ U<:MPolyElem }
  R = parent(f)
  if R != parent(g) 
    error( "Polynomials do not belong to the same ring" )
  end
  S, phi, t = add_variables( R, ["t"] )
  I = ideal(S, [phi(g), one(S)-t[1]*phi(f)])
  G = groebner_basis(I,complete_reduction=true)
  if length(G)==1 && G[1] == one(S)
    return true
  end
  return false
end

function Oscar.radical_membership( f::U, g::U ) where{ U<:MPolyQuoElem }
  P = parent(f)
  R = base_ring(P)
  if P != parent(g) 
    error( "Polynomials do not belong to the same ring" )
  end
  S, phi, t = add_variables( R, ["t"] )
  I = ideal(S, 
	    vcat( [phi(lift(g)), one(S)-t[1]*phi(lift(f))],
		 [phi(h) for h in gens(modulus(P))] )
	    )
  G = groebner_basis(I,complete_reduction=true)
  if length(G)==1 && G[1] == one(S)
    return true
  end
  return false
end

##################################################################
#
# Checks whether some power of g is contained in the principal ideal 
# generated by f and returns a solution (k,a) of the equation 
#    g^k = a*f
# If no such solution exists, it returns (-1,0).
function divides_power( f::T, g::T, check_radical_membership::Bool=false ) where{ T<:MPolyElem }
  println( "Check whether some power of $g is divisible by $f" )
  R = parent(f)
  parent(f) == parent(g) || error( "elements are not contained in the same ring" )
  # Test for radical membership in the first place.
  # TODO: Doesn't this computation already allow us to read 
  # off our desired coefficients? How expensive is this compared 
  # to what follows?
  if check_radical_membership && (!radical_membership( g, f ))
    return (-1, zero(R))
  end

  # Check successivly higher powers of g for containment in ⟨f⟩. 
  # Once such a power is found, try to decrease it again to 
  # find the optimum.

  # the first dummy check that can not be well fit in the inductive 
  # routine below:
  check = false
  a = zero(R)
  check, a = divides( one(parent(f)), f )
  if check
    return (0,a)
  end

  powers = [g]
  while true
    # println( "looking at the $(2^(length(powers)-1))-th power..." )
    check, a = divides( last(powers), f )
    if check
      break
    end
    push!( powers, last(powers)^2 )
  end

  # Now a power g^(2^(l-1)) is found that is divided by g with 
  # l being the length of the vector powers. We use logarithmic 
  # bisection in order to find the optimal power g^k for 
  # which this happens for the first time

  k = 2^(length(powers)-1)
  upper = pop!(powers)
  if length(powers) == 0
    # In this case g = a * f with exponent 1. No need for 
    # further bisection.
    return (1, a)
  end
  l = 2^(length(powers)-1)
  lower = pop!(powers)
  b = one(R)
  while length(powers) > 0
    # println( "looking between the $l-th power and the $k-th power..." )
    power = pop!(powers)
    middle = lower*power
    (check, b) = divides( middle, f )
    if check == true
      upper = middle
      a = b
      k -= 2^length(powers)
    else
      lower = middle
      l += 2^length(powers)
    end
  end
  return (k, a)
end

# TODO: Up to now, this is merely a copy of the above routine. 
function divides_power( f::T, g::T, check_radical_membership::Bool=false ) where{ T<:MPolyQuoElem }
  # println( "Check whether some power of $g is divisible by $f" )
  R = parent(f)
  parent(f) == parent(g) || error( "elements are not contained in the same ring" )

  # Test for radical membership in the first place.
  !check_radical_membership || ( radical_membership( lift(g), modulus(parent(f)) + ideal( parent(lift(f)), lift(f) )) || return (-1, zero(R)))
  # println( "Test for radical membership was positive." )

  # Check successivly higher powers of g for containment in ⟨f⟩. 
  # Once such a power is found, try to decrease it again to 
  # find the optimum.

  # the first dummy check that can not be well fit in the inductive 
  # routine below:
  check = false
  a = zero(R)
  check, a = divides( one(parent(f)), f )
  if check
    return (0,a)
  end

  powers = [g]
  while true
    # println( "looking at the $(2^(length(powers)-1))-th power..." )
    global crash = ( last(powers), f )
    check, a = divides( last(powers), f )
    if check
      break
    end
    push!( powers, last(powers)^2 )
  end

  # Now a power g^(2^(l-1)) is found that is divided by f with 
  # l being the length of the vector powers. We use logarithmic 
  # bisection in order to find the optimal power g^k for 
  # which this happens for the first time

  k = 2^(length(powers)-1)
  upper = pop!(powers)
  if length(powers) == 0
    # In this case g = a * f with exponent 1. No need for 
    # further bisection.
    return (1, a)
  end
  l = 2^(length(powers)-1)
  lower = pop!(powers)
  b = one(R)
  while length(powers) > 0
    # println( "looking between the $l-th power and the $k-th power..." )
    power = pop!(powers)
    middle = lower*power
    (check, b) = divides( middle, f )
    if check == true
      upper = middle
      a = b
      k -= 2^length(powers)
    else
      lower = middle
      l += 2^length(powers)
    end
  end
  return (k, a)
end


@Markdown.doc """
    function diag( d::Vector{MatSpaceElem} )

Writes the matrices in the list d as blocks on the 'diagonal' of 
a new matrix D and returns D. 
"""
function diag( d::Vector )
  if length(d) == 0
    error( "Cannot return a matrix of unspecified type" )
  end
  r = [ ncols(A) for A in d ]
  s = [ ncols(A) for A in d ]
  m = sum(r)
  n = sum(s)
  M = MatrixSpace(base_ring(d[1]),m,n)
  D = zero(M)
  offset_r = 0
  offset_s = 0
  for k in (1:length(d))
    for i in (1:r[k])
      for j in (1:s[k])
	D[offset_r + i, offset_s + j] = d[k][i,j]
      end
    end
    offset_r += r[k]
    offset_s += s[k]
  end
  return D
end

@Markdown.doc """
    as_polynomial_in(f::MPolyElem{BaseRingType}, u::Symbol) where {BaseRingType}

Assumes that `f` is a polynomial in a free polynomial ring 𝕜[x₁,…,xₙ], with `u` being 
one of the variables, say, u=xⱼ. It then returns f as a polynomial in 𝕜[x₁,…,xⱼ₋₁,xⱼ₊₁,…,xₙ][u].
"""
function as_polynomial_in(f::MPolyElem{BaseRingType}, u::MPolyElem{BaseRingType}) where {BaseRingType}
  error( "not implemented, yet" )
end

@Markdown.doc """
    split_polynomial_ring(R::MPolyRing{BaseRingType}, u::Vector{MPolyElem{BaseRingType}}) where {
        BaseRingType}

Assumes that the elements in `u` are generators of the free polynomial ring R over a base
ring 𝕜 of type BaseRingType and writes R as a tower of polynomial rings R ≅ (𝕜[x₁,…,xᵣ])[u₁,…,uₙ]. 
The involved maps are:
  * ι : 𝕜[x₁,…,xᵣ] ↪ (𝕜[x₁,…,xᵣ])[u₁,…,uₙ]
  * ϕ : R → (𝕜[x₁,…,xᵣ])[u₁,…,uₙ]
  * ϕ⁻¹: (𝕜[x₁,…,xᵣ])[u₁,…,uₙ] → R
  * π : (𝕜[x₁,…,xᵣ])[u₁,…,uₙ] → (𝕜[x₁,…,xᵣ])[u₁,…,uₙ]/⟨u₁,…,uₙ⟩ ≅ 𝕜[x₁,…,xᵣ]
"""
function split_polynomial_ring(R::MPolyRing{BaseRingType}, u::Vector{MPolyElem{BaseRingType}}) where {BaseRingType}
  error( "not implemented, yet" )
end

end # of module
