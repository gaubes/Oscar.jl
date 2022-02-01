export IdealSheaf

export scheme, covering, getindex, subscheme, covered_patches, extend!

mutable struct IdealSheaf{
    CoveredSchemeType<:CoveredScheme,
    CoveringType<:Covering,
    SpecType<:Spec,
    RingElemType<:MPolyElem
  }
  X::CoveredSchemeType # Der parent
  C::CoveringType
  ideal_gens::Dict{SpecType, Vector{RingElemType}}

  # fields for caching
  cached_ideals::Dict{SpecType, Any}

  function IdealSheaf(
      X::CoveredSchemeType, 
      C::CoveringType, 
      I::Dict{SpecType, Vector{RingElemType}};
      check::Bool=true
    ) where {
      CoveredSchemeType<:CoveredScheme,
      CoveringType<:Covering,
      SpecType<:Spec,
      RingElemType<:MPolyElem
    }
    if check
      C in coverings(X) || error("reference covering not found")
      for X in keys(I)
        X in patches(C) || error("affine patch not found")
        for f in I[X]
          parent(f) == base_ring(OO(X)) || error("element does not belong to the correct ring")
        end
      end
    end
    return new{CoveredSchemeType, CoveringType, SpecType, RingElemType}(X, C, I)
  end
end

scheme(I::IdealSheaf) = I.X
covering(I::IdealSheaf) = I.C
covered_patches(I::IdealSheaf) = [U for U in keys(I.ideal_gens)]
getindex(I::IdealSheaf, U::Spec) = I.ideal_gens[U]

function setindex!(I::IdealSheaf, g::Vector{RET}, U::Spec) where {RET<:MPolyElem} 
  for f in g
    parent(f) == base_ring(OO(U)) || error("polynomials do not belong to the correct ring")
  end
  I.ideal_gens[U] = g
  return I
end

function IdealSheaf(X::ProjectiveScheme, g::Vector{RingElemType}) where {RingElemType<:MPolyElem_dec}
  X_covered = as_covered_scheme(X)
  C = standard_covering(X)
  r = fiber_dimension(X)
  I = Dict{affine_patch_type(X), Vector{poly_type(affine_patch_type(X))}}()
  for i in 0:r
    I[C[i+1]] = lifted_numerator.(dehomogenize(X, g, i))
  end
  return IdealSheaf(X_covered, C, I, check=false)
end

# this constructs the empty ideal sheaf
function IdealSheaf(X::CoveredScheme) 
  C = default_covering(X)
  D = Dict{affine_patch_type(X), Vector{poly_type(affine_patch_type(X))}}()
  return IdealSheaf(X, C, D, check=false)
end

# internal routine to set up an ideal sheaf by automatic extension 
# from one prescribed set of generators on one affine patch
function IdealSheaf(X::CoveredScheme, C::Covering, U::Spec, g::Vector{RET}) where {RET<:MPolyElem}
  C in coverings(X) || error("the covering does not belong to the scheme")
  U in patches(C) || error("the affine open patch does not belong to the covering")
  for f in g
    parent(f) == base_ring(OO(U)) || error("the generators do not belong to the correct ring")
  end
  D = Dict{typeof(U), typeof(g)}()
  D[U] = g
  I = IdealSheaf(X, C, D)
  extend!(I)
  return I
end

# more user facing routine with an automatic lookup of the associated affine patch.
# Warning: This might be ambiguous and lead to unexpected results since one affine 
# patch can be used in different coverings!
function IdealSheaf(X::CoveredScheme, g::Vector{RET}) where {RET<:MPolyElem} 
  length(g) == 0 && IdealSheaf(X)
  R = parent(g[1])
  for i in 2:length(g)
    R == parent(g[i]) || error("elements do not belong to the same ring")
  end
  for C in coverings(X)
    for U in patches(C)
      R == base_ring(OO(U)) && return IdealSheaf(X, C, U, g)
    end
  end
  error("the given set of generators could not be associated to an affine patch of the scheme")
end

function IdealSheaf(X::CoveredScheme, C::Covering, g::Vector{RET}) where {RET<:MPolyElem} 
  length(g) == 0 && IdealSheaf(X)
  R = parent(g[1])
  for i in 2:length(g)
    R == parent(g[i]) || error("elements do not belong to the same ring")
  end
  C in coverings(X) || error("covering is not listed")
  for U in patches(C)
    R == base_ring(OO(U)) && return IdealSheaf(X, C, U, g)
  end
  error("the given set of generators could not be associated to an affine patch of the scheme")
end



### Given an ideal sheaf I, return the associated 
# subscheme
#
# **Note:** This must be cached!
function subscheme(I::IdealSheaf) 
  X = scheme(I)
  C = covering(I)
  new_patches = [subscheme(C[i], I[C[i]]) for i in 1:npatches(C)]
  new_glueings = Dict{Tuple{affine_patch_type(C), affine_patch_type(C)}, glueing_type(C)}()
  for (U, V) in keys(glueings(C))
    i = C[U]
    j = C[V]
    Unew = new_patches[i]
    Vnew = new_patches[j]
    G = C[U, V]
    new_glueings[(Unew, Vnew)] = restriction(C[U, V], Unew, Vnew, check=false)
    new_glueings[(Vnew, Unew)] = inverse(new_glueings[(Unew, Vnew)])
  end
  Cnew = Covering(new_patches, new_glueings)
  return CoveredScheme(Cnew)
end


@Markdown.doc """
    extend!(I::IdealSheaf)

For ``I`` an ideal sheaf on a covered scheme ``X``, given with respect 
to a covering `C = {Uᵢ}` with ``I`` defined only on a subset of 
the affine patches, this extends the ideal sheaf to all of ``X``.

This proceeds by crawling through the glueing graph and taking 
closures in the patches ``Uⱼ`` of the subschemes 
``Zᵢⱼ = V(I) ∩ Uᵢ ∩ Uⱼ`` in the intersection with a patch ``Uᵢ`` 
on which ``I`` had already been described.
"""
function extend!(I::IdealSheaf)
  X = scheme(I)
  C = covering(I)
  gg = glueing_graph(C)
  # push all nodes on which I is known in a heap
  dirty_patches = covered_patches(I)
  while length(dirty_patches) > 0
    U = pop!(dirty_patches)
    N = neighbor_patches(C, U)
    Z = subscheme(U, I[U])
    for V in N
      # check whether this node already knows about I
      V in covered_patches(I) && continue

      # if not, extend I to this patch
      f, _ = glueing_morphisms(C[V, U])
      ZV = closure(preimage(f, Z))
      I[V] = gens(defining_ideal(ZV))
      V in dirty_patches || push!(dirty_patches, V)
    end
  end
  # TODO: Extend trivially to disjoint components?
  return I
end

function Base.show(io::IO, I::IdealSheaf)
  print(io, "sheaf of ideals on $(scheme(I))")
end