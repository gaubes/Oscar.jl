########################################################################
#
# Cocomplexes for Abelian groups.
#
# This should be used with any type of Abelian groups such as 
# finite ℤ-modules, vector spaces over a field 𝕜, or finitely generated 
# modules over a ring R. To this end we require the input to satisfy 
# the common requirements of an Abelian category. 
#
#   * The cochain groups are supposed to be of type `GroupType` which 
#     can be anything. 
#     Note that there can be many different concrete types for one 
#     category. For instance, for the category of modules over a ring 
#     R, it is customary to have types for free modules, 
#     for finitely presented modules, for submodules, etc. 
#     We will assume that for every chain cocomplex C, all entries have 
#     the same type. But different chain cocomplexes may have entries 
#     of different types.
#   * There must be a type `GroupHomType` for homomorphisms from  
#     objects of type `GroupType1` and `GroupType2`. For the boundary 
#     maps in one cocomplex the latter two types are assumed to 
#     be the same.
#   * It must be possible to compose homomorphisms of type 
#     `GroupHomType`, i.e. there must be a function 
#
#       compose(::GroupHomType1, ::GroupHomType2).
#
#   * There must be a constructor for the zero object of the given 
#     category. Unfortunately, we can not derive this from a type 
#     directly, but we must provide another concrete instance of 
#     an object in the same category, i.e.
#
#       zero_object(::GroupType)::GroupType
#
#     must be implemented for every `GroupType` that is being used.
#   * For any concrete instances of `GroupType1` and `GroupType2` 
#     there must be a constructor for the zero morphism:
#
#       zero_morphism(::GroupType1, ::GroupType2)
#
#   * For a homomorphism f : C → D with C of type `GroupType1` 
#     and D of type `GroupType2` there must be the functionality 
#
#       kernel(f)::KernelType
#       image(f)::ImageType
#
#     returning the kernel and the image as subgroups of C and D, 
#     respectively.
#   * For a group Z and a subgroup B ⊂ Z there must be the function
#
#       quo(Z, B)::QuotientType
#
#     returning the quotient Z/B.
#
#   * There must be direct sums a vector of groups:
#
#       direct_sum(::Vector{GroupType})
#
#     for all types of groups that are being used. 
#     For a vector c = (C₁,…,Cₙ) this must return a triple 
#
#       (C₁ ⊕ … ⊕ Cₙ, (i₁,…,iₙ), (p₁,…, pₙ))
#
#     consisting of the direct sum C₁ ⊕ … ⊕ Cₙ, the injections 
#
#       iₖ : Cₖ ↪ C₁ ⊕ … ⊕ Cₙ
#
#     and the projections 
#
#       pₖ : C₁ ⊕ … ⊕ Cₙ → Cₖ.
#
#   * There must be the Hom-functor for pairs of groups 
#
#       Hom(::GroupType1, ::GroupType2)::GroupType3
#
#     taking a pair (C, D) to Hom(C, D).
#
########################################################################

import Base: last, length, first
export ChainCocomplex, BoundedCocomplex
export differential, differentials, shift, kernels, cochains, boundaries, cohomologies, kernel, boundary, cohomology

export ChainCocomplexHom, BoundedCocomplexHom
export domain, codomain, maps, shift, getindex, mapping_cone

abstract type ChainCocomplex{GroupType, GroupHomType} end

########################################################################
# 
# Bounded cochain complexes of Abelian groups.
#
# These store a finite number of Abelian groups Xⁱ together with 
# morphisms dⁱ: Xⁱ→ Xⁱ⁺¹ and fills up all remaining entries with 
# zeroes. Example:
#
# math. index:   -5   -4   -3   -2   -1    0    1    2    3
#
# groups:         0    X⁻⁴→ X⁻³→ X⁻²→ X⁻¹→ X⁰→  X¹   0    0
#
# intern. index:  -    1    2    3    4    5    6    -    - 
# 
# Internally, the groups and morphisms are stored in vectors 
# starting with the `internal index` 1. For the translation from 
# the mathematical to the internal index, we also store a shift, 
# which is 5 in the above case.
#
########################################################################

mutable struct BoundedCocomplex{GroupType, GroupHomType} <: ChainCocomplex{GroupType, GroupHomType}
  shift::Int
  C::Vector{GroupType}
  d::Vector{GroupHomType}

  # fiends for caching
  Z::Vector
  B::Vector
  H::Vector

  function BoundedCocomplex(C::Vector{GroupType}, d::Vector{GroupHomType}; shift::Int=1, check::Bool=false) where {GroupType, GroupHomType}
    n = length(C)
    n > 0 || error("need at least one instance of an object")
    length(d) == n-1 || error("number of maps does not coincide with the number of groups")
    for i in 1:n-1
      domain(d[i]) == C[i] || error("domain of definition does not coincide with the given element")
      codomain(d[i]) == C[i+1] || error("codomain of definition does not coincide with the given element")
    end
    if check
      for i in 1:n-2
	iszero(compose(d[i], d[i+1])) || error("composition of the differentials does not equate to zero")
      end
    end

    return new{GroupType, GroupHomType}(shift, C, d)
  end
end

function getindex(C::BoundedCocomplex, i::Int)
  if i+shift(C) < 1 
    return zero_object(first(C))
  end
  if i+shift(C)> length(cochains(C))
    return zero_object(last(C))
  end
  return C.C[i+C.shift]
end

differentials(C::BoundedCocomplex) = C.d

function differential(C::BoundedCocomplex, i::Int) 
  if i+shift(C) < 1 || i+shift(C) >= length(cochains(C))
    return zero_morphism(C[i], C[i+1])
  end
  return C.d[i+C.shift]
end

length(C::BoundedCocomplex) = length(C.C)
last(C::BoundedCocomplex) = last(C.C)
first(C::BoundedCocomplex) = C.C[1]
shift(C::BoundedCocomplex) = C.shift
shift(C::BoundedCocomplex, i::Int) = BoundedCocomplex(cochains(C), differentials(C), shift=shift(C)+i, check=false)

cochains(C::BoundedCocomplex) = C.C

function kernels(C::BoundedCocomplex)
  if !isdefined(C, :Z)
    C.Z = Vector()
    for i in 1:length(C)-1
      push!(C.Z, kernel(d[i]))
    end
    push!(C.Z, last(C))
  end
  return C.Z
end

function boundaries(C::BoundedCocomplex)
  if !isdefined(C, :B)
    C.B = Vector()
    push!(C.B, zero_object(first(C)))
    for i in 2:length(C)
      push!(C.B, image(d[i]))
    end
  end
  return C.B
end

function cohomologies(C::BoundedCocomplex)
  if !isdefined(C, :H)
    C.H = Vector()
    Z = kernels(C)
    B = boundaries(C)
    for i in 1:length(C)
      push!(C.H, quo(Z[i], B[i]))
    end
  end
  return C.H
end

function kernel(C::BoundedCocomplex, i::Int) 
  if i+shift(C) < 1 || i+shift(C) > length(cochains(C))
    return C[i] # just the zero object
  end
  return kernels(C)[i+shift(C)]
end

function boundary(C::BoundedCocomplex, i::Int) 
  if i+shift(C) < 1 || i+shift(C) > length(cochains(C))
    return C[i] # just the zero object
  end
  return boundaries(C)[i+shift(C)]
end

function cohomology(C::BoundedCocomplex, i::Int) 
  if i+shift(C) < 1 || i+shift(C) > length(cochains(C))
    return C[i] # just the zero object
  end
  return cohomologies(C)[i+shift(C)]
end


abstract type ChainCocomplexHom{DomainGroupType, DomainGroupHomType, CodomainGroupType, CodomainGroupHomType, TransitionHomType} end

mutable struct BoundedCocomplexHom{DGT, DGHT, CGT, CGHT, THT} <: ChainCocomplexHom{DGT, DGHT, CGT, CGHT, THT}
  domain::BoundedCocomplex{DGT, DGHT}
  codomain::BoundedCocomplex{CGT, CGHT}
  maps::Vector{THT}
  map_shift::Int

  # We assume that the maps in `maps` start in 
  # the first index where both C and D are nonzero 
  # and stop at the first index where either C or D 
  # vanishes.
  #
  #    -5   -4   -3   -2   -1    0    1    2    3
  #
  #     0    X⁻⁴→ X⁻³→ X⁻²→ X⁻¹→ X⁰→  X¹   0    0
  #
  #                    ↓    ↓    ↓    ↓    
  #
  #     0    0    0    Y⁻²→ Y⁻¹→ Y⁰→  Y¹→  Y²   0
  #
  # The shift of X is 5, the shift of Y is 3. 
  # Then the shift for the vector of maps is 3 and 
  # the length is 4. All other maps are zero, anyway.
  function BoundedCocomplexHom(
      C::BoundedCocomplex{DGT, DGHT},
      D::BoundedCocomplex{CGT, CGHT},
      maps::Vector{THT};
      check::Bool=false
  ) where {DGT, DGHT, CGT, CGHT, THT}
    @show C
    @show D
    @show maps
    n = length(maps)
    @show n
    map_shift = minimum([shift(C), shift(D)])
    @show shift(C)
    @show shift(D)
    @show map_shift
    for i in 1:n
      @show i
      @show domain(maps[i]) 
      @show C[i-map_shift] 
      domain(maps[i]) == C[i-map_shift] || error("wrong domain")
      codomain(maps[i]) == D[i-map_shift] || error("wrong codomain")
    end
    
    if check
      # perform the checks on commutativity of the squares
    end

    return new{DGT, DGHT, CGT, CGHT, THT}(C, D, maps, map_shift)
  end
end

domain(f::BoundedCocomplexHom) = f.domain
codomain(f::BoundedCocomplexHom) = f.codomain
maps(f::BoundedCocomplexHom) = f.maps 
shift(f::BoundedCocomplexHom) = f.map_shift

function getindex(f::BoundedCocomplexHom, i::Int)
  if i+shift(f) < 1 || i+shift(f) > length(maps(f)) 
    return zero_morphism(domain(f)[i], codomain(f)[i])
  end
  return maps(f)[i+shift(f)]
end

function mapping_cone(f::BoundedCocomplexHom)
  C = domain(f)
  D = codomain(f)
  start_index = -maximum([shift(C), shift(D)])
  end_index = maximum([length(C)-shift(C), length(D)-shift(D)])
  C_shift = shift(C, 1)
  @show start_index
  @show end_index
  cochains_and_maps = [direct_sum([D[i], C_shift[i]]) for i in start_index:end_index]
  cochains = [b[1] for b in cochains_and_maps]
  @show cochains
  inclusions = [b[2] for b in cochains_and_maps]
  projections = [b[3] for b in cochains_and_maps]
  @show [matrix.(p) for p in projections]
  @show [matrix.(i) for i in inclusions]
  for i in start_index:end_index-1
    @show i
    @show compose(projections[i-start_index+1][1], differential(D, i))
    #@show domain(compose(projections[i-start_index+1][1], differential(D, i)))
    #@show codomain(compose(projections[i-start_index+1][1], differential(D, i)))

    @show compose(projections[i-start_index+1][2], f[i+1])
    #@show domain(compose(projections[i-start_index+1][2], f[i+1]))
    #@show codomain(compose(projections[i-start_index+1][2], f[i+1]))

    @show inclusions[i+1-start_index+1][1]
    #@show domain(inclusions[i+1-start_index+1][1])
    #@show codomain(inclusions[i+1-start_index+1][1])
    
    @show compose(
	      compose(projections[i-start_index+1][1], differential(D, i)) + 
	        compose(projections[i-start_index+1][2], f[i+1]), 
	      inclusions[i+1-start_index+1][1]
	      ) 

    @show compose(projections[i-start_index+1][1], zero_morphism(D[i], C[i+2])) 
    #@show domain(compose(projections[i-start_index+1][1], zero_morphism(D[i], C[i+2])) )
    #@show codomain(compose(projections[i-start_index+1][1], zero_morphism(D[i], C[i+2])) )

    @show compose(projections[i-start_index+1][2], -differential(C, i+1))
    #@show domain(compose(projections[i-start_index+1][2], -differential(C, i+1)))
    #@show codomain(compose(projections[i-start_index+1][2], -differential(C, i+1)))

    @show inclusions[i+1-start_index+1][2]
    #@show domain(inclusions[i+1-start_index+1][2])
    #@show codomain(inclusions[i+1-start_index+1][2])
    
    @show compose(
	      compose(projections[i-start_index+1][1], zero_morphism(D[i], C[i+2])) + 
	        compose(projections[i-start_index+1][2], -differential(C, i+1)),
	      inclusions[i+1-start_index+1][2]
              ) 
    @show compose(
	      compose(projections[i-start_index+1][1], differential(D, i)) + 
	        compose(projections[i-start_index+1][2], f[i+1]), 
	      inclusions[i+1-start_index+1][1]
	      ) + 
      compose(
	      compose(projections[i-start_index+1][1], zero_morphism(D[i], C[i+2])) + 
	        compose(projections[i-start_index+1][2], -differential(C, i+1)),
	      inclusions[i+1-start_index+1][2]
              ) 
  end
  coboundary_maps = [
      compose(
	      compose(projections[i-start_index+1][1], differential(D, i)) + 
	        compose(projections[i-start_index+1][2], f[i+1]), 
	      inclusions[i+1-start_index+1][1]
	      ) + 
      compose(
	      compose(projections[i-start_index+1][1], zero_morphism(D[i], C[i+2])) + 
	        compose(projections[i-start_index+1][2], -differential(C, i+1)),
	      inclusions[i+1-start_index+1][2]
              ) 
      for i in start_index:end_index-1]
  cone = BoundedCocomplex(cochains, coboundary_maps, shift=-start_index+1, check=false)
  map_to = BoundedCocomplexHom(D, cone, [b[1] for b in inclusions[2:length(inclusions)]])
  map_from = BoundedCocomplexHom(cone, C_shift, [b[2] for b in projections])
  return cone, map_to, map_from
end

