```@meta
CurrentModule = Oscar
DocTestSetup = quote
  using Oscar
end
```

```@setup oscar
using Oscar
```

```@contents
Pages = ["subgroups.md"]
```

# [Subgroups](@id subgroups)

The following functions are available in Oscar for subgroup properties:

```@docs
sub(G::GAPGroup, gens::AbstractVector{<:GAPGroupElem})
issubgroup
embedding(G::T, H::T) where T <: GAPGroup
index(G::T, H::T) where T <: GAPGroup
isnormal(G::T, H::T) where T <: GAPGroup
ischaracteristic(G::T, H::T) where T <: GAPGroup
```

## Standard subgroups

The following functions are available in Oscar to obtain standard subgroups of
a group `G`. Every such function returns a tuple `(H,f)`, where `H` is a group
of the same type of `G` and `f` is the embedding homomorphism of `H` into `G`.

```@docs
trivial_subgroup
centre
sylow_subgroup(G::GAPGroup, p::Int64)
derived_subgroup
fitting_subgroup
frattini_subgroup
radical_subgroup
socle
pcore(G::GAPGroup, p::Int64)
intersect(V::T...) where T<:GAPGroup
```

The following functions return a vector of subgroups.

```@docs
subgroups(G::GAPGroup)
normal_subgroups
maximal_subgroups
maximal_normal_subgroups
minimal_normal_subgroups
characteristic_subgroups
derived_series
sylow_system
hall_subgroups_representatives(G::GAPGroup, P::AbstractVector{<:Base.Integer})
hall_system
complement_system
```

!!! note
    When a function returns a vector of subgroups,
    the output consists in the subgroups only;
    the embeddings are not returned as well.
    To get the embedding homomorphism of the subgroup `H` in `G`,
    one can type `embedding(G,H)`.


## Conjugation action of elements and subgroups

```@docs
isconjugate(G::GAPGroup, x::GAPGroupElem, y::GAPGroupElem)
isconjugate(G::GAPGroup, H::GAPGroup, K::GAPGroup)
representative_action(G::GAPGroup, x::GAPGroupElem, y::GAPGroupElem)
representative_action(G::GAPGroup, H::GAPGroup, K::GAPGroup)
centralizer(G::GAPGroup, x::GAPGroupElem)
centralizer(G::T, H::T) where T <: GAPGroup
normalizer(G::GAPGroup, x::GAPGroupElem)
normalizer(G::T, H::T) where T<:GAPGroup
core(G::T, H::T) where T<:GAPGroup
normal_closure(G::T, H::T) where T<:GAPGroup
GroupConjClass{T<:GAPGroup, S<:Union{GAPGroupElem,GAPGroup}}
conjugacy_class(G::GAPGroup, g::GAPGroupElem)
conjugacy_class(G::T, g::T) where T<:GAPGroup
conjugacy_classes(G::GAPGroup)
conjugacy_classes_subgroups(G::GAPGroup)
conjugacy_classes_maximal_subgroups(G::GAPGroup)
```


## Cosets (left/right/double)

```@docs
GroupCoset
right_coset(H::GAPGroup, g::GAPGroupElem)
left_coset(H::GAPGroup, g::GAPGroupElem)
isright(c::GroupCoset)
isleft(c::GroupCoset)
isbicoset(C::GroupCoset)
acting_domain(C::GroupCoset)
representative(C::GroupCoset)
right_cosets(G::GAPGroup, H::GAPGroup)
left_cosets(G::GAPGroup, H::GAPGroup)
right_transversal(G::T, H::T) where T<: GAPGroup
left_transversal(G::T, H::T) where T<: GAPGroup
GroupDoubleCoset{T <: GAPGroup, S <: GAPGroupElem}
double_coset(G::T, g::GAPGroupElem{T}, H::T) where T<: GAPGroup
double_cosets(G::T, H::T, K::T; NC=false) where T<: GAPGroup
left_acting_group(C::GroupDoubleCoset)
right_acting_group(C::GroupDoubleCoset)
representative(C::GroupDoubleCoset)
order(C::Union{GroupCoset,GroupDoubleCoset})
Base.rand(C::Union{GroupCoset,GroupDoubleCoset})
intersect(V::AbstractVector{Union{T, GroupCoset, GroupDoubleCoset}}) where T <: GAPGroup
```
