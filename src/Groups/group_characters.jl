##  This is a first attempt to implement group characters in Oscar.
##  
##  The idea is that the available GAP objects (groups, character tables,
##  class functions) are used in a first step, and that access to character
##  values yields `QabElem` objects.
##  
##  Once we agree on the functionality and the integration into Oscar,
##  this setup can in a second step be replaced by one that uses
##  native Julia objects for representing class functions,
##  but character tables and groups still have some counterpart in GAP.
##  
##  In a third step, we replace the character table objects by native Julia
##  objects.

# character values are elements from QabField

import Base: getindex, length, mod, one, print, show, zero

import Oscar.AbelianClosure: QabElem, QabAutomorphism

import Nemo: degree

export
    character_table,
    decomposition_matrix,
    scalar_product,
    trivial_character


#############################################################################
##
##  conversion between Julia's QabElem and GAP's cyclotomics
##
function QabElem(cyc::GapInt)
    GAP.Globals.IsCyc(cyc) || error("cyc must be a GAP cyclotomic")
    denom = GAP.Globals.DenominatorCyc(cyc)
    n = GAP.Globals.Conductor(cyc)
    coeffs = GAP.Globals.ExtRepOfObj(cyc * denom)
    cycpol = GAP.Globals.CyclotomicPol(n)
    dim = length(cycpol)-1
    GAP.Globals.ReduceCoeffs(coeffs, cycpol)
    coeffs = Vector{fmpz}(coeffs)
    coeffs = coeffs[1:dim]
    denom = fmpz(denom)
    F, z = Nemo.CyclotomicField(n)
    val = Nemo.elem_from_mat_row(F, Nemo.matrix(Nemo.ZZ, 1, dim, coeffs), 1, denom)
    return QabElem(val, n)
end

function gap_cyclotomic(elm::QabElem)
    coeffs = [Nemo.coeff(elm.data, i) for i in 0:(elm.c-1)]  # fmpq
    return GAP.Globals.CycList(GAP.GapObj(coeffs; recursive=true))
end

complex_conjugate(elm::QabElem) = elm^QabAutomorphism(-1)


#############################################################################
##
##  character tables
##
abstract type GroupCharacterTable end

mutable struct GAPGroupCharacterTable <: GroupCharacterTable
    GAPGroup::Oscar.GAPGroup    # the underlying group, if any
    GAPTable::GAP.GapObj  # the character table object
    characteristic::Int
    AbstractAlgebra.@declare_other

    function GAPGroupCharacterTable(G::Oscar.GAPGroup, tab::GAP.GapObj, char::Int)
      ct = new()
      ct.GAPGroup = G
      ct.GAPTable = tab
      ct.characteristic = char
      return ct
    end

    function GAPGroupCharacterTable(tab::GAP.GapObj, char::Int)
      ct = new()
      #ct.GAPGroup is left undefined
      ct.GAPTable = tab
      ct.characteristic = char
      return ct
    end
end

"""
    character_table(G::Oscar.GAPGroup, p::Int = 0)

Return the ordinary (if `p == 0`) or `p`-modular character table of the
finite group `G`.
If the `p`-modular character table of `G` cannot be computed by GAP
then `nothing` is returned.

# Examples
```jldoctest
julia> character_table( symmetric_group(3) )
Sym( [ 1 .. 3 ] )

 2  1  1  .
 3  1  .  1
           
   1a 2a 3a
2P 1a 1a 3a
3P 1a 2a 1a
           
χ₁  1 -1  1
χ₂  2  . -1
χ₃  1  1  1


julia> character_table( symmetric_group(3), 2 )
Sym( [ 1 .. 3 ] )

 2  1  .
 3  1  1
        
   1a 3a
2P 1a 3a
3P 1a 1a
        
χ₁  1  1
χ₂  2 -1


```
"""
function character_table(G::Oscar.GAPGroup, p::Int = 0)
    tbls = AbstractAlgebra.get_special(G, :character_tables)
    if tbls == nothing
      tbls = Dict()
      AbstractAlgebra.set_special(G, :character_tables => tbls)
    end

    return get!(tbls, p) do
      gaptbl = GAP.Globals.CharacterTable(G.X)
      if p != 0
        # Create the `p`-modular table if possible.
        isprime(p) || error("p must be 0 or a prime integer")
        gaptbl = GAP.Globals.mod(gaptbl, GAP.Obj(p))
        gaptbl == GAP.Globals.fail && return nothing
      end
      return GAPGroupCharacterTable(G, gaptbl, p)
    end
end

# A character table with stored group object is stored in this group.
# Character tables from the table library do not store groups,
# they are cached in the dictionary `character_tables_by_id`,
# in order to achieve that fetching the same table twice yields the same
# object.
const character_tables_by_id = Dict{String, Union{GAPGroupCharacterTable, Nothing}}()

"""
    character_table(id::String, p::Int = 0)

Return the ordinary (if `p == 0`) or `p`-modular character table
for which `id` is an admissible name in GAP's library of character tables.
If no such table is available then `nothing` is returned.

# Examples
```jldoctest
julia> println(character_table("A5"))
character_table("A5")

julia> println(character_table("A5", 2))
character_table("A5mod2")

julia> println(character_table("J5"))
nothing

```
"""
function character_table(id::String, p::Int = 0)
    hasproperty(GAP.Globals, :CTblLib) || error("no character table library available")

    if p == 0
      modid = id
    else
      isprime(p) || error("p must be 0 or a prime integer")
      modid = id * "mod" * string(p)
    end

    return get!(character_tables_by_id, modid) do
      tbl = GAP.Globals.CharacterTable(GAP.GapObj(modid))
      tbl == GAP.Globals.fail && return nothing
      return GAPGroupCharacterTable(tbl, p)
    end
end

##############################################################################
#
# admissible names of library character tables

function all_character_table_names()
    K = GAP.Globals.CallFuncList(GAP.Globals.AllCharacterTableNames, GAP.GapObj([]))
    return Vector{String}(K)
end
#TODO:
# Support function/value pairs as arguments, similar to (but more general
# than) `all_small_groups` etc.
# This makes sense only if GAP's Browse package is available (and has been
# loaded at the time when the character table library got loaded),
# otherwise everything is too slow.
# Currently this cannot be assumed.


##############################################################################
#
# `print` and `show` character tables

# Utility:
# Create strings in length-lexicographical ordering w.r.t. the
# alphabet 'alphabet'.
# (If `alphabet` is `"ABCDEFGHIJKLMNOPQRSTUVWXYZ"` then the strings
# have the form `"A", "B", ..., "Z", "AA", ...`.)
mutable struct WordsIterator
    alphabet::String
end

Base.iterate(wi::WordsIterator) = length(wi.alphabet) == 0 ? nothing : (string(wi.alphabet[1]), 2)

function Base.iterate(wi::WordsIterator, state::Int)
    name = ""
    n = state
    ll = length(wi.alphabet)
    while 0 < n
      n, r = divrem(n-1, ll)
      name = wi.alphabet[r+1] * name
    end
    return (name, state+1)
end


# Utility:
# Turn integer values to strings, but replace `0` by `"."`,
# print irrationalities via the `Hecke.math_html` method for `nf_elem`.
# If `alphabet` is nonempty then represent irrationalities by names
# generated by iterating over `WordsIterator(alphabet)`,
# and store the meanings of these strings in the form of pairs `value => name`
# in the legend array that is returned.
function matrix_of_strings(tbl::GAPGroupCharacterTable; alphabet::String = "")
  n = nrows(tbl)
  m = Array{String}(undef, n, n)
  legend = []
  if alphabet != ""
    iter = WordsIterator(alphabet)
    state = 1
  end

  # Run column-wise through the matrix of irreducibles.
  # The same is done in GAP, thus the names are compatible with the ones
  # created by GAP,
  # except that relative names in the case of complex conjugation and
  # quadratic irrationalities are currently not handled here.
  for j in 1:n
    for i in 1:n
      val = tbl[i,j]
      if iszero(val)
        m[i, j] = "."
      elseif val.c == 1
        m[i, j] = string(val.data)
      elseif alphabet != ""
        # write irrationalities using symbols
        pos = findnext(x -> x[1] == val, legend, 1)
        if pos != nothing
          m[i,j] = legend[pos][2]
        else
          pos = findnext(x -> x[1] == -val, legend, 1)
          if pos != nothing
            m[i,j] = "-" * legend[pos][2]
          else
            name, state = iterate(iter, state)
            push!(legend, val => name)
            valbar = complex_conjugate(val)
            if valbar != val
              push!(legend, valbar => "\\overline{"*name*"}")
#TODO: represent the unique Galois conjugate of `A` different from `A` by `A*`,
#      and show both `A` and `A*` in the footer;
#      for elements in quadratic fields, show also an expression in terms of
#      square roots in the footer.
            end
            m[i,j] = name
          end
        end
      else
        # write irrationalities in terms of `\zeta`
        m[i,j] = sprint(Hecke.math_html, val.data)
      end
    end
  end
  return (m, legend)
end

# Produce LaTeX output if `"text/html"` is prescribed,
# via the `:TeX` attribute of the io context.
function Base.show(io::IO, ::MIME"text/html", tbl::GAPGroupCharacterTable)
  print(io, "\$")
  show(IOContext(io, :TeX => true), tbl)
  print(io, "\$")
end

# Produce a screen format without LaTeX markup but with unicode characters
# and sub-/superscripts if LaTeX output is not requested..
function Base.show(io::IO, tbl::GAPGroupCharacterTable)
    n = nrows(tbl)
    gaptbl = tbl.GAPTable
    size = GAP.gap_to_julia(GAP.Globals.Size(gaptbl))
    primes = [x[1] for x in collect(factor(size))]
    sort!(primes)

    # Decide how to deal with irrationalities.
    alphabet = get(io, :alphabet, "")
    if alphabet == ""
      with_legend = get(io, :with_legend, false)
      if with_legend == true
        alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      end
    end

    # Create the strings of the values of the irreducibles.
    mat, legend = matrix_of_strings(tbl, alphabet = alphabet)

    # Compute the factored centralizer orders.
    cents = GAP.gap_to_julia(GAP.Globals.SizesCentralizers(gaptbl))
    fcents = [collect(factor(x)) for x in cents]
    d = Dict([p => fill(".", n) for p in primes]...)
    for i in 1:n
      for pair in fcents[i]
        d[pair[1]][i] = string(pair[2])
      end
    end
    cents_strings = [d[p] for p in primes]

    # Compute display format for power maps.
    names = Vector{String}(GAP.Globals.ClassNames(gaptbl))
    pmaps = Vector{Any}(GAP.Globals.ComputedPowerMaps(gaptbl))
    power_maps_primes = String[]
    power_maps_strings = Vector{String}[]
    for i in 2:length(pmaps)
      map = pmaps[i]
      if map != nothing
        push!(power_maps_primes, string(i)*"P")
        push!(power_maps_strings, names[map])
      end
    end

    empty = ["" for i in 1:n]

    if isdefined(tbl, :GAPGroup)
      headerstring = string(tbl.GAPGroup)
    else
      headerstring = String(GAP.Globals.Identifier(gaptbl))
    end

    # Create the IO context.
    ioc = IOContext(io,
      # header (an array of strings):
      # name of the table and separating empty line
      :header => [headerstring, ""],

      # column labels:
      # centralizer orders (factored),
      # separating empty line,
      # class names,
      # separating empty line,
      # p-th power maps for known p-th power maps,
      # separating empty line,
      :labels_col => permutedims(hcat(
        cents_strings..., empty, names, power_maps_strings..., empty)),

      # row labels:
      # character names (a column vector is sufficient)
      :labels_row => ["\\chi_{" * string(i) * "}" for i in 1:n],

      # corner (a column vector is sufficient):
      # primes in the centralizer rows,
      # separating empty line,
      # separating empty line,
      # primes in the power map rows,
      # separating empty line,
      :corner => vcat( string.(primes), ["", ""], power_maps_primes, [""] ),

      # footer (an array of strings)
      :footer => length(legend) == 0 ? [] :
                 vcat([""],
                      [pair[2] * " = " * sprint(Hecke.math_html, pair[1].data)
                       for pair in legend]),
    )

    # print the table
    labelled_matrix_formatted(ioc, mat)
end

# print: abbreviated form
function Base.print(io::IO, tbl::GAPGroupCharacterTable)
    gaptbl = tbl.GAPTable
    if isdefined(tbl, :GAPGroup)
      id = string(tbl.GAPGroup)
      if tbl.characteristic != 0
        id = id * " mod " * string(tbl.characteristic)
      end
    else
      id = "\"" * String(GAP.Globals.Identifier(gaptbl)) * "\""
    end
    print(io, "character_table($id)")
end


##############################################################################
#
length(tbl::GAPGroupCharacterTable) = GAP.Globals.NrConjugacyClasses(tbl.GAPTable)
Oscar.nrows(tbl::GAPGroupCharacterTable) = GAP.Globals.NrConjugacyClasses(tbl.GAPTable)
Oscar.ncols(tbl::GAPGroupCharacterTable) = GAP.Globals.NrConjugacyClasses(tbl.GAPTable)

function Base.getindex(tbl::GAPGroupCharacterTable, i::Int)
    return group_class_function(tbl, GAP.Globals.Irr(tbl.GAPTable)[i])
end
#TODO: cache the irreducibles in the table

function Base.getindex(tbl::GAPGroupCharacterTable, i::Int, j::Int)
    val = GAP.Globals.Irr(tbl.GAPTable)[i, j]
    return QabElem(val)
end
#TODO: cache the values once they are known?

Base.iterate(tbl::GAPGroupCharacterTable, state = 1) = state > nrows(tbl) ? nothing : (tbl[state], state+1)

"""
    mod(tbl::GAPGroupCharacterTable, p::Int)

Return the `p`-modular character table of `tbl`,
or `nothing` if this table cannot be computed.

An exception is thrown if `tbl` is not an ordinary character table.
"""
function Base.mod(tbl::GAPGroupCharacterTable, p::Int)
    isprime(p) || error("p must be a prime integer")
    tbl.characteristic == 0 || error("tbl mod p only for ordinary table tbl")

    modtbls = AbstractAlgebra.get_special(tbl, :brauer_tables)
    if modtbls == nothing
      modtbls = Dict{Int,Any}()
      AbstractAlgebra.set_special(tbl, :brauer_tables => modtbls)
    end
    if ! haskey(modtbls, p)
      modtblgap = mod(tbl.GAPTable, p)
      if modtblgap == GAP.Globals.fail
        modtbls[p] = nothing
      elseif isdefined(tbl, :GAPGroup)
        modtbls[p] = GAPGroupCharacterTable(tbl.GAPGroup, modtblgap, p)
      else
        modtbls[p] = GAPGroupCharacterTable(modtblgap, p)
      end
    end

    return modtbls[p]
end

"""
    decomposition_matrix(modtbl::GAPGroupCharacterTable)

Return the decomposition matrix (of type `fmpz_mat`) of the Brauer character
table `modtbl`.
The rows and columns are indexed by the irreducible characters of the ordinary
character table of `modtbl` and the irreducible characters of `modtbl`,
respectively,

# Examples
```jldoctest
julia> t = character_table("A5"); t2 = mod(t, 2);

julia> decomposition_matrix(t2)
[1   0   0   0]
[1   0   1   0]
[1   1   0   0]
[0   0   0   1]
[1   1   1   0]

```
"""
function decomposition_matrix(modtbl::GAPGroupCharacterTable)
    isprime(modtbl.characteristic) || error("characteristic of tbl must be a prime integer")
    return fmpz_mat(GAP.Globals.DecompositionMatrix(modtbl.GAPTable))
end


#############################################################################
##
##  class functions (and characters)
##
abstract type GroupClassFunction end
#TODO: support character rings and elements of it?
#      if yes then there is no need to have class other functions than these

struct GAPGroupClassFunction <: GroupClassFunction
    table::GAPGroupCharacterTable
    values::GAP.GapObj
end

function Base.show(io::IO, chi::GAPGroupClassFunction)
    print(io, "group_class_function(" * string(chi.table) * ", " * string(values(chi)) * ")")
end

import Base.values

function values(chi::GAPGroupClassFunction)
    gapvalues = GAP.Globals.ValuesOfClassFunction(chi.values)
    return [QabElem(x) for x in gapvalues]
end

function group_class_function(tbl::GAPGroupCharacterTable, values::GAP.GapObj)
    GAP.Globals.IsClassFunction(values) || error("values must be a class function")
    return GAPGroupClassFunction(tbl, values)
end

function group_class_function(tbl::GAPGroupCharacterTable, values::Vector{QabElem})
    gapvalues = GAP.GapObj([gap_cyclotomic(x) for x in values])
    return GAPGroupClassFunction(tbl, GAP.Globals.ClassFunction(tbl.GAPTable, gapvalues))
end

function group_class_function(G::Oscar.GAPGroup, values::Vector{QabElem})
    return group_class_function(character_table(G), values)
end

function trivial_character(tbl::GAPGroupCharacterTable)
    val = QabElem(1)
    return group_class_function(tbl, [val for i in 1:ncols(tbl)])
end

function trivial_character(G::Oscar.GAPGroup)
    val = QabElem(1)
    return group_class_function(G, [val for i in 1:GAP.Globals.NrConjugacyClasses(G.X)])
end

Base.length(chi::GAPGroupClassFunction) = length(chi.values)

Base.iterate(chi::GAPGroupClassFunction, state = 1) = state > length(chi.values) ? nothing : (chi[state], state+1)

# the degree is an fmpq
# (for general class functions, denominators can occur)
function Nemo.degree(chi::GAPGroupClassFunction)
    val = values(chi)[1]
    return Nemo.coeff(val.data, 0)
end

# access character values
Base.getindex(chi::GAPGroupClassFunction, i::Int) = QabElem(GAP.Globals.ValuesOfClassFunction(chi.values)[i])

function Base.:(==)(chi::GAPGroupClassFunction, psi::GAPGroupClassFunction)
    chi.table === psi.table || error("character tables must be identical")
#T check_parent?
    return chi.values == psi.values
end

# aritmetics with class functions
function Base.:+(chi::GAPGroupClassFunction, psi::GAPGroupClassFunction)
    chi.table === psi.table || error("character tables must be identical")
    return GAPGroupClassFunction(chi.table, chi.values + psi.values)
end

Base.:-(chi::GAPGroupClassFunction) = GAPGroupClassFunction(chi.table, - chi.values)

function Base.:-(chi::GAPGroupClassFunction, psi::GAPGroupClassFunction)
    chi.table === psi.table || error("character tables must be identical")
    return GAPGroupClassFunction(chi.table, chi.values - psi.values)
end

function Base.:*(chi::GAPGroupClassFunction, psi::GAPGroupClassFunction)
    chi.table === psi.table || error("character tables must be identical")
    return GAPGroupClassFunction(chi.table, chi.values * psi.values)
end

function Base.zero(chi::GAPGroupClassFunction)
    val = QabElem(0)
    return group_class_function(chi.table, [val for i in 1:length(chi)])
end

Base.one(chi::GAPGroupClassFunction) = trivial_character(chi.table)

function scalar_product(chi::GAPGroupClassFunction, psi::GAPGroupClassFunction)
    chi.table === psi.table || error("character tables must be identical")
    return Nemo.fmpz(GAP.Globals.ScalarProduct(chi.values, psi.values))
end

function Base.:*(n::IntegerUnion, chi::GAPGroupClassFunction)
    return GAPGroupClassFunction(chi.table, n * chi.values)
end

function Base.:^(chi::GAPGroupClassFunction, n::IntegerUnion)
    return GAPGroupClassFunction(chi.table, chi.values ^ n)
end

# apply a class function to a group element
function(chi::GAPGroupClassFunction)(g::BasicGAPGroupElem)
    # Identify the conjugacy class of `g`.
    ccl = GAP.Globals.ConjugacyClasses(GAP.Globals.UnderlyingGroup(chi.table.GAPTable))
    for i in 1:length(ccl)
      if g.X in ccl[i]
        return chi[i]
      end
    end
    error("$g is not an element in the underlying group")
end
